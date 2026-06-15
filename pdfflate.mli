(** Interface to miniz.c via Zlib-like functions

    This is very slightly modified from Leroy's CamlZip. *)

(** Raised on an error in either compression or decompression. *)
exception Error of string * string

(** The type of zlib streams. *)
type stream

(** Initialise a zlib inflate stream. *)
val inflate_stream: ?header: bool -> unit -> stream

(** Initialise a zlib deflate stream. *)
val deflate_stream: ?level: int -> ?header: bool -> unit -> stream

(** Reset an inflate stream. *)
val inflate_reset : stream -> unit

(** Reset a deflate stream. *)
val deflate_reset : stream -> unit

(** Compress data. The [string -> int] function is an input: given a buffer, it
writes some data to it, returning the number of bytes written. The [string ->
int -> unit] function is an output: giving a buffer and a number of compressed
bytes written. The optional argument [level] gives the zlib compression level
(the default is 6). The optional argument [header] will, if [true], output a
zlib header (the default is [true]). *)
val compress:
  ?level: int -> ?header: bool -> ?stream: stream ->
  (bytes -> int) -> (bytes -> int -> unit) -> unit

(** Uncompress data. The input and output functions are as described for
[compress]. If [header] is [true], a zlib header is expected (the default is
[true]). *)
val uncompress:
  ?header: bool -> ?stream: stream -> (bytes -> int) -> (bytes -> int -> unit) -> unit
