(** Gets the MIME type based on the file extension. *)
val get_mime_type : string -> string

(** Determines if the given MIME type should be compressed. *)
val should_compress : string -> bool
