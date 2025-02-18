open Core
open Lwt.Infix
open Thumper__Response

let get_mime_type filepath =
  match
    String.lowercase
      (match String.rsplit2 ~on:'.' filepath with
       | Some (_, ext) -> "." ^ ext
       | None -> "")
  with
  | ".html" -> "text/html; charset=utf-8"
  | ".css" -> "text/css"
  | ".js" -> "application/javascript"
  | ".json" -> "application/json"
  | ".png" -> "image/png"
  | ".jpg" | ".jpeg" -> "image/jpeg"
  | ".gif" -> "image/gif"
  | ".svg" -> "image/svg+xml"
  | ".pdf" -> "application/pdf"
  | ".wasm" -> "application/wasm"
  | _ -> "application/octet-stream"
;;

let should_compress mime_type =
  let mime_type = String.lowercase mime_type in
  (* Strip charset and other parameters *)
  let base_mime_type =
    match String.split mime_type ~on:';' with
    | [] -> ""
    | first :: _ -> String.strip first
  in
  (* Common compressible MIME types *)
  let compressible_types =
    [ "text/"
    ; (* All text types *)
      "application/json"
    ; (* JSON *)
      "application/javascript"
    ; (* JavaScript *)
      "application/ecmascript"
    ; (* ECMAScript *)
      "application/x-javascript"
    ; (* Old JavaScript type *)
      "application/xml"
    ; (* XML *)
      "application/x-yaml"
    ; (* YAML *)
      "application/ld+json"
    ; (* JSON-LD *)
      "application/graphql" (* GraphQL *)
    ]
  in
  List.exists compressible_types ~f:(fun prefix ->
      String.is_prefix base_mime_type ~prefix)
;;

module Server = struct
  type connection =
    { client_sock : UnixLabels.file_descr
    ; client_addr : Core_unix.sockaddr
    }

  type request_method =
    | GET
    | POST
    | PUT
    | DELETE
    | HEAD
    | OPTIONS
    | CONNECT
    | TRACE
    | PATCH
  [@@deriving show, enum, sexp, compare]

  type request =
    { method_ : request_method [@key "method"]
    ; route : string
    ; headers : (string * string) list
    ; body : string
    }
  [@@deriving sexp]

  type route_fn = request -> Lwt_io.output_channel -> status_code Lwt.t

  type t =
    { mutable conns : connection list
    ; mutable running : bool
    ; listener : UnixLabels.file_descr option
    ; routes : (string * request_method, route_fn) Hashtbl.Poly.t
    ; mutable not_found : route_fn
    ; mutable error : route_fn
    }

  let request_method_of_string s =
    match String.uppercase s with
    | "GET" -> Ok GET
    | "POST" -> Ok POST
    | "PUT" -> Ok PUT
    | "DELETE" -> Ok DELETE
    | "HEAD" -> Ok HEAD
    | "OPTIONS" -> Ok OPTIONS
    | "CONNECT" -> Ok CONNECT
    | "TRACE" -> Ok TRACE
    | "PATCH" -> Ok PATCH
    | _ -> Error (Printf.sprintf "Invalid request method: %s" s)
  ;;

  let parse_header header_string =
    let items = String.split_lines header_string |> List.to_array in
    match Array.length items with
    | 0 -> Error "Empty header"
    | _ ->
      let method_and_route = items.(0) |> String.split ~on:' ' |> List.to_array in
      (match Array.length method_and_route with
       | x when x < 2 -> Error "Invalid request line"
       | _ ->
         (match request_method_of_string method_and_route.(0) with
          | Error e -> Error e
          | Ok method_ ->
            let route = method_and_route.(1) in
            let rec parse_request_headers items acc =
              match items with
              | [] -> acc
              | hd :: tl ->
                let key, value = String.lsplit2_exn ~on:':' hd in
                parse_request_headers tl ((key, value) :: acc)
            in
            let headers =
              parse_request_headers
                (Array.sub ~pos:1 ~len:(Array.length items - 1) items |> Array.to_list)
                []
            in
            Ok { method_; route; headers; body = "" }))
  ;;

  let parse_msg message =
    let contents = Str.split (Str.regexp_string "\r\n\r\n") message |> List.to_array in
    match Array.length contents with
    | x when x < 1 -> Error "Invalid message"
    | x ->
      let header = contents.(0) in
      (match parse_header header with
       | Error e -> Error e
       | Ok request ->
         if x > 1 then Ok { request with body = contents.(1) } else Ok request)
  ;;

  (** Initialize the buffer to 0 *)
  let __init_buffer (buf : bytes) size = Bytes.fill buf ~pos:0 ~len:size '\000'

  let accept_gzip_encoding headers =
    List.exists headers ~f:(fun (name, value) ->
        let name = String.strip name in
        let value = String.strip value in
        (* Debug each header being checked *)
        if String.Caseless.equal name "accept-encoding"
        then Spice.debugf "Found Accept-Encoding header with value: '%s'" value;
        String.Caseless.equal name "accept-encoding"
        && String.is_substring ~substring:"gzip" value)
  ;;

  let gzip_compress (input : string) : string =
    let compressed = Ezgzip.compress input in
    compressed
  ;;

  let write_response
      status_code
      body
      oc
      ?headers
      ?(compression = true)
      ?(request_headers = [])
      ()
    =
    let base_headers =
      match headers with
      | None -> []
      | Some h -> h
    in
    let response_parts =
      [ Printf.sprintf "HTTP/1.1 %s" (Thumper__Response.to_string status_code) ]
    in
    (* Check compression conditions *)
    let body, headers =
      match base_headers with
      | _ when not compression -> body, base_headers
      | headers ->
        let content_type =
          List.find headers ~f:(fun (k, _) -> String.Caseless.equal k "content-type")
          |> Option.map ~f:snd
          |> Option.value ~default:""
        in
        let accepts = accept_gzip_encoding request_headers in
        let should_compress = should_compress content_type in
        Spice.debugf "Accepts=%b ; Should compress=%b" accepts should_compress;
        if accepts && should_compress
        then (
          let compressed = gzip_compress body in
          let compressed_size = String.length compressed in
          let compressed_headers =
            ("Content-Encoding", "gzip")
            :: ("Content-Length", Int.to_string compressed_size)
            :: ("Vary", "Accept-Encoding")
            :: List.filter headers ~f:(fun (k, _) ->
                not (String.Caseless.equal k "content-length"))
          in
          compressed, compressed_headers)
        else body, headers
    in
    let response_parts =
      let headers_strings =
        List.fold headers ~init:[] ~f:(fun acc (key, data) ->
            Printf.sprintf "%s: %s" key data :: acc)
      in
      response_parts @ headers_strings
    in
    let response_parts = response_parts @ [ ""; body ] in
    let response = String.concat ~sep:"\r\n" response_parts in
    Lwt_io.write oc response >>= fun () -> Lwt_io.flush oc
  ;;

  let not_found (_req : request) oc =
    let data =
      {|
    <doctype html>
    <body>
    <h1> 404 Not Found </h1>
    </body>
    |}
    in
    let headers = [ "Content-Type", "text/html; charset=utf-8" ] in
    let status = S404_Not_Found in
    write_response status data oc ~headers () >>= fun () -> Lwt.return status
  ;;

  let server_error (_req : request) oc =
    let data =
      {|
    <doctype html>
    <body>
    <h1> 500 Internal Server Error </h1>
    </body>
    |}
    in
    let headers = [ "Content-Type", "text/html; charset=utf-8" ] in
    let status = S500_Internal_Server_Error in
    write_response status data oc ~headers () >>= fun () -> Lwt.return status
  ;;

  let server =
    ref
      { conns = []
      ; running = false
      ; listener = None
      ; routes = Hashtbl.Poly.create ()
      ; not_found
      ; error = server_error
      }
  ;;

  type cache_content =
    { content : string
    ; mime_type : string
    ; size : string
    ; mtime : float
    }

  let static_file_cache = Hashtbl.Poly.create ()

  let cache_file filepath =
    let get_file_content () =
      let stat = Core_unix.stat filepath in
      let content = In_channel.read_all filepath in
      let mime_type = get_mime_type filepath in
      { content; mime_type; size = Int64.to_string stat.st_size; mtime = stat.st_mtime }
    in
    match Hashtbl.find static_file_cache filepath with
    | Some cached ->
      (try
         let stat = Core_unix.stat filepath in
         if Float.(stat.st_mtime > cached.mtime)
         then (
           let new_cached_data = get_file_content () in
           Hashtbl.set static_file_cache ~key:filepath ~data:new_cached_data;
           Lwt.return (Some new_cached_data))
         else Lwt.return (Some cached)
       with
       | _ -> Lwt.return (Some cached))
    | None ->
      (try
         let cached_data = get_file_content () in
         Hashtbl.Poly.set static_file_cache ~key:filepath ~data:cached_data;
         Lwt.return (Some cached_data)
       with
       | _ -> failwith "Failed to cache file")
  ;;

  let set_error_fn error_fn = !server.error <- error_fn
  let set_not_found_fn not_found_fn = !server.not_found <- not_found_fn

  let find_matching_route (request : request) =
    let rec find_match (items : ((string * request_method) * route_fn) list) =
      match items with
      | [] -> None
      | ((route, method_), handler) :: rest ->
        if compare_request_method method_ request.method_ = 0
        then
          if String.equal route request.route
          then Some handler (* Exact match *)
          else if String.is_suffix route ~suffix:"*"
          then (
            let prefix = String.drop_suffix route 1 in
            if String.is_prefix request.route ~prefix
            then Some handler
            else find_match rest)
          else find_match rest
        else find_match rest
    in
    !server.routes |> Hashtbl.Poly.to_alist |> find_match
  ;;

  let rec read_headers acc ic =
    Lwt.catch
      (fun () ->
         Lwt_io.read_line ic
         >>= fun line ->
         if String.is_empty line
         then Lwt.return (List.rev acc)
         else read_headers (line :: acc) ic)
      (fun _ -> Lwt.return (List.rev acc))
  ;;

  let read_body ic content_length =
    if content_length > 0
    then (
      let buffer = Bytes.create content_length in
      Lwt.catch
        (fun () ->
           Lwt_io.read_into ic buffer 0 content_length
           >>= fun bytes_read ->
           if bytes_read = content_length
           then Lwt.return (Some (Bytes.to_string buffer))
           else Lwt.return None)
        (fun _ -> Lwt.return None))
    else Lwt.return (Some "")
  ;;

  let handle_connection client_sock (_client_addr : Core_unix.sockaddr) =
    Lwt.catch
      (fun () ->
         Lwt.return (Lwt_io.of_fd ~mode:Lwt_io.Input client_sock)
         >>= fun input_channel ->
         Lwt.return (Lwt_io.of_fd ~mode:Lwt_io.Output client_sock)
         >>= fun output_channel ->
         read_headers [] input_channel
         >>= fun headers ->
         match headers with
         | [] -> Lwt.return_unit
         | request_line :: header_lines ->
           (match
              parse_msg (String.concat ~sep:"\r\n" (request_line :: header_lines))
            with
            | Ok request ->
              (match find_matching_route request with
               | None ->
                 !server.not_found request output_channel
                 >>= fun response ->
                 Spice.errorf
                   "%s - %s - %s"
                   request.route
                   (sexp_of_request_method request.method_ |> Sexp.to_string)
                   (Thumper__Response.to_string response);
                 Lwt.return_unit
               | Some route_fn ->
                 let start_time = Core.Time_float.now () in
                 route_fn request output_channel
                 >>= fun status ->
                 let delta = Core.Time_float.diff (Core.Time_float.now ()) start_time in
                 Spice.infof
                   "%s - %s [%f ms] - %s"
                   request.route
                   (sexp_of_request_method request.method_ |> Sexp.to_string)
                   (Core.Time_float.Span.to_ms delta)
                   (Thumper__Response.to_string status);
                 Lwt.return_unit)
            | Error msg ->
              Spice.errorf "Error parsing message: %s" msg;
              Lwt.return_unit)
           >>= fun () -> Lwt.return ())
      (fun e ->
         Printf.eprintf "Exception in handle_connection: %s\n" (Exn.to_string e);
         Lwt.return ())
    >>= fun () ->
    Lwt.catch (fun () -> Lwt_unix.close client_sock) (fun _ -> Lwt.return_unit)
  ;;

  let add_route route route_fn =
    !server.routes |> Hashtbl.Poly.add_exn ~key:route ~data:route_fn
  ;;

  let start_server port =
    let listen_address = Core_unix.ADDR_INET (Core_unix.Inet_addr.localhost, port) in
    let server_socket = Lwt_unix.socket Core_unix.PF_INET Core_unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt server_socket Lwt_unix.SO_REUSEADDR true;
    let _ = Lwt_unix.bind server_socket listen_address in
    Lwt_unix.listen server_socket 10;
    let rec accept_connection () =
      Lwt.catch
        (fun () ->
           Lwt_unix.accept server_socket
           >>= fun (client_sock, client_addr) ->
           Lwt.async (fun () -> handle_connection client_sock client_addr);
           accept_connection ())
        (fun exn ->
           Printf.eprintf "Error accepting connection: %s\n" (Exn.to_string exn);
           accept_connection ())
    in
    Spice.infof "Server started on port %d" port;
    accept_connection ()
  ;;

  let serve_static_file ?mime_type filepath route =
    let%bind _ = cache_file filepath in
    let route_handler request oc =
      let%bind cached_value = cache_file filepath in
      match cached_value with
      | Some cached ->
        let content_type =
          match mime_type with
          | Some m -> m
          | None -> cached.mime_type
        in
        let headers =
          [ "Content-Type", content_type
          ; "Vary", "Accept-Encoding"
          ; "Cache-Control", "max-age=31536000"
          ; "ETag", Printf.sprintf "%Lx-%f" (Int64.of_string cached.size) cached.mtime
          ]
        in
        let header_string =
          List.fold headers ~init:"" ~f:(fun acc (key, value) ->
              acc ^ Printf.sprintf "%s: %s\r\n" key value)
        in
        let response =
          Printf.sprintf "HTTP/1.1 200 OK\r\n%s\r\n%s" header_string cached.content
        in
        Lwt_io.write oc response
        >>= fun () -> Lwt_io.flush oc >>= fun () -> Lwt.return S200_OK
      | None -> !server.not_found request oc
    in
    add_route (route, GET) route_handler;
    Lwt.return_unit
  ;;

  let serve_static_directory dirpath route_prefix =
    let normalize_path path =
      let parts =
        String.split path ~on:'/'
        |> List.filter ~f:(fun p ->
            (not (String.equal p ".."))
            && (not (String.equal p "."))
            && not (String.equal p ""))
      in
      String.concat ~sep:"/" parts
    in
    let rec cache_directory dir =
      let%bind items =
        try Lwt_unix.files_of_directory dir |> Lwt_stream.to_list with
        | _ -> Lwt.return []
      in
      Lwt_list.iter_s
        (fun entry ->
           (* Skip "." and ".." explicitly *)
           if String.equal entry "." || String.equal entry ".."
           then Lwt.return_unit
           else (
             let full_path = Filename.concat dir entry in
             Lwt.catch
               (fun () ->
                  Lwt_unix.lstat full_path
                  >>= fun stat ->
                  let kind = stat.Lwt_unix.st_kind in
                  match kind with
                  | Lwt_unix.S_REG ->
                    let%bind _ = cache_file full_path in
                    Lwt.return_unit
                  | Lwt_unix.S_DIR ->
                    (* Skip symbolic links to avoid infinite loops *)
                    (match kind with
                     | Lwt_unix.S_LNK -> cache_directory full_path
                     | _ -> Lwt.return_unit)
                  | _ -> Lwt.return_unit)
               (fun _ -> Lwt.return_unit)))
        items
    in
    let%bind () = cache_directory dirpath in
    let add_dir_routes prefix =
      let route_handler request oc =
        let request_path =
          let prefix_len = String.length prefix in
          if prefix_len = 0
          then request.route
          else if String.length request.route > prefix_len
          then String.drop_prefix request.route prefix_len
          else ""
        in
        let normalized_path = normalize_path request_path in
        let filepath = Filename.concat dirpath normalized_path in
        let%bind cached_value = cache_file filepath in
        match cached_value with
        | Some cached ->
          let headers =
            [ "Content-Type", cached.mime_type
            ; "Vary", "Accept-Encoding"
            ; "Cache-Control", "max-age=31536000"
            ; "ETag", Printf.sprintf "%Lx-%f" (Int64.of_string cached.size) cached.mtime
            ]
          in
          let%bind _ =
            write_response
              S200_OK
              cached.content
              oc
              ~headers
              ~compression:true
              ~request_headers:request.headers
              ()
          in
          Lwt.return S200_OK
        | None -> !server.not_found request oc
      in
      if not (String.is_suffix prefix ~suffix:"/*")
      then add_route (prefix ^ "/*", GET) route_handler;
      Lwt.return_unit
    in
    add_dir_routes route_prefix
  ;;

  let stream content_type stream oc =
    let initial_response =
      Printf.sprintf
        "HTTP/1.1 200 OK\r\nContent-Type: %s\r\nTransfer-Encoding: chunked\r\n\r\n"
        content_type
    in
    Lwt_io.write oc initial_response
    >>= fun () ->
    Lwt_io.flush oc
    >>= fun () ->
    Lwt_stream.iter_s
      (fun chunk_data ->
         let chunk =
           Printf.sprintf "%x\r\n%s\r\n" (String.length chunk_data) chunk_data
         in
         Lwt_io.write oc chunk >>= fun () -> Lwt_io.flush oc)
      stream
    >>= fun () ->
    Lwt_io.write oc "0\r\n\r\n"
    >>= fun () -> Lwt_io.flush oc >>= fun () -> Lwt.return S200_OK
  ;;
end
