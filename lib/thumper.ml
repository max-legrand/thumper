open Core

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
