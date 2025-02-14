open Core
open Thumper__Response

(** Gets the MIME type based on the file extension. *)
val get_mime_type : string -> string

(** Determines if the given MIME type should be compressed. *)
val should_compress : string -> bool

module Server : sig
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

  (** Converts a string to a request method. *)
  val request_method_of_string : string -> (request_method, string) Result.t

  (** Parses the header string into a request. *)
  val parse_header : string -> (request, string) Result.t

  (** Parses the entire message into a request. *)
  val parse_msg : string -> (request, string) Result.t

  (** [__init_buffer buf size] Initializes the buffer [buf] of size [size] to 0. *)
  val __init_buffer : bytes -> int -> unit

  (** Checks if the headers accept gzip encoding. *)
  val accept_gzip_encoding : (string * string) list -> bool

  (** Compresses the input string using gzip. *)
  val gzip_compress : string -> string

  (** Writes the response to the output channel. *)
  val write_response
    :  status_code
    -> string
    -> Lwt_io.output_channel
    -> ?headers:(string * string) list
    -> ?compression:bool
    -> ?request_headers:(string * string) list
    -> unit
    -> unit Lwt.t

  (** Handles the case when a route is not found. *)
  val not_found : request -> Lwt_io.output_channel -> status_code Lwt.t

  (** Handles server errors. *)
  val server_error : request -> Lwt_io.output_channel -> status_code Lwt.t

  (** Sets the function to handle not found errors. *)
  val set_not_found_fn : route_fn -> unit

  (** Sets the function to handle server errors. *)
  val set_error_fn : route_fn -> unit

  type cache_content =
    { content : string
    ; mime_type : string
    ; size : string
    ; mtime : float
    }

  (** Caches a static file. *)
  val cache_file : string -> cache_content option Lwt.t

  (** Finds a matching route for the given request. *)
  val find_matching_route : request -> route_fn option

  (** Reads headers from the input channel. *)
  val read_headers : string list -> Lwt_io.input_channel -> string list Lwt.t

  (** Reads the body from the input channel. *)
  val read_body : Lwt_io.input_channel -> int -> string option Lwt.t

  (** Handles a connection. *)
  val handle_connection : Lwt_unix.file_descr -> Core_unix.sockaddr -> unit Lwt.t

  (** Adds a route to the server. *)
  val add_route : string * request_method -> route_fn -> unit

  (** Starts the server on the given port. *)
  val start_server : int -> unit Lwt.t

  (** Serves a static file. *)
  val serve_static_file : ?mime_type:string -> string -> string -> unit Lwt.t

  (** Serves a static directory. *)
  val serve_static_directory : string -> string -> unit Lwt.t

  val stream : string -> string Lwt_stream.t -> Lwt_io.output_channel -> status_code Lwt.t
end
