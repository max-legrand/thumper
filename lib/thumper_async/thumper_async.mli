open Async
open Core
open Thumper__Response

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

  type route_fn = request -> Writer.t -> status_code Deferred.t

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

  (** Writes the response to the writer. *)
  val write_response
    :  status_code
    -> string
    -> Writer.t
    -> ?headers:(string * string) list
    -> ?compression:bool
    -> ?request_headers:(string * string) list
    -> unit
    -> unit Deferred.t

  (** Handles the case when a route is not found. *)
  val not_found : request -> Writer.t -> status_code Deferred.t

  (** Handles server errors. *)
  val server_error : request -> Writer.t -> status_code Deferred.t

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
  val cache_file : string -> cache_content option Deferred.t

  (** Finds a matching route for the given request. *)
  val find_matching_route : request -> route_fn option

  (** Reads headers from the reader. *)
  val read_headers : string list -> Reader.t -> string list Deferred.t

  (** Reads the body from the reader. *)
  val read_body : Reader.t -> int -> string option Deferred.t

  (** Handles a connection. *)
  val handle_connection : Reader.t -> Writer.t -> unit Deferred.t

  (** Adds a route to the server. *)
  val add_route : string * request_method -> route_fn -> unit

  (** Starts the server on the given port. *)
  val start_server : int -> unit Deferred.t

  (** Serves a static file. *)
  val serve_static_file : ?mime_type:string -> string -> string -> unit Deferred.t

  (** Serves a static directory. *)
  val serve_static_directory : string -> string -> unit Deferred.t

  val stream : string -> string Pipe.Reader.t -> Writer.t -> status_code Deferred.t
end
