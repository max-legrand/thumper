open Core
open Async
open Thumper
open Thumper__Response

module Config = struct
  type static_route_resource_type =
    | FILE
    | DIRECTORY

  type t =
    { port : int
    ; routes : (string * static_route_resource_type * string * string option) list
    }

  let parse_config file =
    let data = In_channel.read_all file in
    let yaml =
      match Yaml.of_string data with
      | Ok yaml -> yaml
      | Error _ -> failwith "Failed to parse config file"
    in
    let port =
      Yaml.Util.find_exn "port" yaml
      |> Option.value_exn
      |> Yaml.Util.to_float_exn
      |> Float.to_int
    in
    let routes = Yaml.Util.find_exn "routes" yaml |> Option.value_exn in
    let routes_value =
      Yaml.Util.keys_exn routes
      |> List.map ~f:(fun key ->
          let value = Yaml.Util.find_exn key routes |> Option.value_exn in
          match Yaml.Util.find_exn "file" value with
          | None ->
            (match Yaml.Util.find_exn "directory" value with
             | None -> failwith "Invalid route"
             | Some d -> key, DIRECTORY, Yaml.Util.to_string_exn d, None)
          | Some f ->
            let custom_mime =
              match Yaml.Util.find_exn "mime" value with
              | None -> None
              | Some m -> Some (Yaml.Util.to_string_exn m)
            in
            key, FILE, Yaml.Util.to_string_exn f, custom_mime)
    in
    { port; routes = routes_value }
  ;;
end

let serve_test (req : Server.request) conn =
  let data =
    `Assoc
      [ "test", `String "Hello, World!"
      ; "test2", `List [ `String "OCaml"; `String "&"; `String "Svelte" ]
      ]
  in
  let body = Yojson.Safe.to_string data in
  let headers = [ "Content-Type", "application/json; charset=utf-8" ] in
  let%bind _ =
    Server.write_response S200_OK body conn ~headers ~request_headers:req.headers ()
  in
  return S200_OK
;;

let not_found (_req : Server.request) conn =
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
  let%bind _ = Server.write_response status data conn ~headers () in
  return status
;;

let ise (_req : Server.request) conn =
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
  let%bind _ = Server.write_response status data conn ~headers () in
  return status
;;

let main () =
  let config = Config.parse_config "example/config.yaml" in
  let%bind () =
    Deferred.List.iter
      config.routes
      ~how:`Sequential
      ~f:(fun (route, resource, filepath, custom_mime) ->
          match resource with
          | Config.FILE ->
            (match custom_mime with
             | Some m -> Server.serve_static_file ~mime_type:m filepath route
             | None -> Server.serve_static_file filepath route)
          | Config.DIRECTORY -> Server.serve_static_directory filepath route)
  in
  Server.set_not_found_fn not_found;
  Server.set_error_fn ise;
  Server.add_route ("/test", Server.GET) serve_test;
  Server.start_server config.port >>| fun () -> Ok ()
;;

let () =
  don't_wait_for
    (main ()
     >>| function
     | Ok () -> ()
     | Error e ->
       Spice.errorf "Error: %s" (Exn.to_string e);
       shutdown 1);
  never_returns (Scheduler.go ())
;;
