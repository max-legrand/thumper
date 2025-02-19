open Thumper
open Core
open Async
open Thumper__Response

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

  type route_fn = request -> Writer.t -> status_code Deferred.t

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
         if x > 1
         then (
           let body =
             String.concat
               ~sep:"\r\n\r\n"
               (Array.sub ~pos:1 ~len:(Array.length contents - 1) contents
                |> Array.to_list)
           in
           Ok { request with body })
         else (
           let content_length =
             List.find_map request.headers ~f:(function
                 | "Content-Length", value -> Some (Int.of_string value)
                 | _ -> None)
           in
           match content_length with
           | Some length ->
             let body = String.sub message ~pos:(String.length header + 4) ~len:length in
             Ok { request with body }
           | None -> Ok request))
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
      writer
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
    Writer.write writer response;
    Writer.flushed writer
  ;;

  (* let not_found (_request : request) writer = *)
  (*   let body = In_channel.read_all "templates/not_found.html" in *)
  (*   let headers = [ "Content-Type", "text/html; charset=utf-8" ] in *)
  (*   let%bind () = write_response Response.S404_Not_Found body writer ~headers () in *)
  (*   return Response.S404_Not_Found *)
  (* ;; *)
  (**)
  (* let server_error (_request : request) writer = *)
  (*   let body = In_channel.read_all "templates/server_error.html" in *)
  (*   let headers = [ "Content-Type", "text/html; charset=utf-8" ] in *)
  (*   let%bind _ = *)
  (*     write_response Response.S500_Internal_Server_Error body writer ~headers () *)
  (*   in *)
  (*   return Response.S500_Internal_Server_Error *)
  (* ;; *)

  let not_found (_req : request) writer =
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
    let%bind _ = write_response status data writer ~headers () in
    return status
  ;;

  let server_error (_req : request) writer =
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
    let%bind _ = write_response status data writer ~headers () in
    return status
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
           return (Some new_cached_data))
         else return (Some cached)
       with
       | _ -> return (Some cached))
    | None ->
      (try
         let cached_data = get_file_content () in
         Hashtbl.Poly.set static_file_cache ~key:filepath ~data:cached_data;
         return (Some cached_data)
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

  let rec read_headers acc reader =
    let%bind line = Reader.read_line reader in
    match line with
    | `Eof -> return (List.rev acc)
    | `Ok line ->
      if String.is_empty line
      then return (List.rev acc)
      else read_headers (line :: acc) reader
  ;;

  let read_body reader content_length =
    if content_length > 0
    then (
      let buffer = Bytes.create content_length in
      match%bind Reader.read reader ~pos:0 ~len:content_length buffer with
      | `Eof -> return None
      | `Ok bytes_read when bytes_read = content_length ->
        return (Some (Bytes.to_string buffer))
      | `Ok bytes_read ->
        Spice.errorf "Incomplete read: got %d of %d bytes" bytes_read content_length;
        return None)
    else return (Some "")
  ;;

  let handle_connection reader writer =
    let%bind headers = read_headers [] reader in
    match headers with
    | [] -> return ()
    | request_line :: header_lines ->
      let content_length =
        List.find_map headers ~f:(fun line ->
            if String.is_prefix ~prefix:"Content-Length:" line
            then (
              let value = String.drop_prefix line 15 in
              let stripped = String.strip value in
              Some (Int.of_string stripped))
            else None)
        |> Option.value ~default:0
      in
      (* Read the empty line after headers *)
      let%bind _ = Reader.read_line reader in
      (* Read body if present *)
      let%bind body = read_body reader content_length in
      let full_request =
        String.concat ~sep:"\r\n" (request_line :: header_lines)
        ^ "\r\n\r\n"
        ^ Option.value body ~default:""
      in
      (match parse_msg full_request with
       | Ok request ->
         (match find_matching_route request with
          | None ->
            let%bind response = !server.not_found request writer in
            Spice.errorf
              "%s - %s - %s"
              request.route
              (sexp_of_request_method request.method_ |> Sexp.to_string)
              (Thumper__Response.to_string response);
            return ()
          | Some route_fn ->
            let start_time = Core.Time_float.now () in
            let%bind status = route_fn request writer in
            let delta = Core.Time_float.diff (Core.Time_float.now ()) start_time in
            Spice.infof
              "%s - %s [%f ms] - %s"
              request.route
              (sexp_of_request_method request.method_ |> Sexp.to_string)
              (Core.Time_float.Span.to_ms delta)
              (Thumper__Response.to_string status);
            return ())
       | Error msg ->
         Spice.errorf "Error parsing message: %s" msg;
         return ())
  ;;

  let add_route route route_fn =
    !server.routes |> Hashtbl.Poly.add_exn ~key:route ~data:route_fn
  ;;

  let max_concurrent_jobs = 100
  let connection_semaphore = Throttle.create ~max_concurrent_jobs ~continue_on_error:true

  let start_server port =
    let%bind _tcp_server =
      Tcp.Server.create
        ~on_handler_error:`Raise
        (Tcp.Where_to_listen.of_port port)
        (fun _addr reader writer ->
           Throttle.enqueue connection_semaphore (fun () ->
               handle_connection reader writer))
    in
    Spice.infof "Server started on port %d" port;
    Deferred.never ()
  ;;

  let serve_static_file ?mime_type filepath route =
    let%map _ = cache_file filepath in
    let route_handler request writer =
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
        let%bind _ =
          write_response
            S200_OK
            cached.content
            writer
            ~headers
            ~compression:true
            ~request_headers:request.headers
            ()
        in
        return S200_OK
      | None -> !server.not_found request writer
    in
    add_route (route, GET) route_handler
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
        try Sys.readdir dir with
        | _ -> return [||]
      in
      Deferred.List.iter ~how:`Sequential (Array.to_list items) ~f:(fun entry ->
          let full_path = Filename.concat dir entry in
          match Core_unix.stat full_path with
          | stat ->
            let kind = stat.st_kind |> Core_unix.sexp_of_file_kind |> Sexp.to_string in
            let reg_kind =
              Core_unix.S_REG |> Core_unix.sexp_of_file_kind |> Sexp.to_string
            in
            if String.equal kind reg_kind
            then (
              let%bind _ = cache_file full_path in
              return ())
            else if String.equal kind "S_DIR"
            then cache_directory full_path
            else return ()
          | exception _ -> return ())
    in
    let%bind () = cache_directory dirpath in
    let add_dir_routes prefix =
      let route_handler request writer =
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
              writer
              ~headers
              ~compression:true
              ~request_headers:request.headers
              ()
          in
          return S200_OK
        | None -> !server.not_found request writer
      in
      if not (String.is_suffix prefix ~suffix:"/*")
      then add_route (prefix ^ "/*", GET) route_handler;
      return ()
    in
    add_dir_routes route_prefix
  ;;

  let stream content_type stream writer =
    let initial_response =
      Printf.sprintf
        "HTTP/1.1 200 OK\r\nContent-Type: %s\r\nTransfer-Encoding: chunked\r\n\r\n"
        content_type
    in
    Writer.write writer initial_response;
    let%bind () = Writer.flushed writer in
    Pipe.iter stream ~f:(fun chunk_data ->
        let chunk = Printf.sprintf "%x\r\n%s\r\n" (String.length chunk_data) chunk_data in
        Writer.write writer chunk;
        Writer.flushed writer)
    >>= fun () ->
    Writer.write writer "0\r\n\r\n";
    let%bind () = Writer.flushed writer in
    return S200_OK
  ;;
end
