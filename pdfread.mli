(** Reading PDF Files *)

(** Read a PDF from a [Pdfio.input], with an optional user password which, if
absent, is assumed to be the empty string, and optional owner password. *)
val pdf_of_input : ?revision:int -> string option -> string option -> Pdfio.input -> Pdf.t

(** Same as [pdf_of_input], but delay loading of streams and parsing of objects
(they will be loaded and parsed when needed). Useful if we only intend to do
something simple, like read metadata.  *)
val pdf_of_input_lazy : ?revision:int -> string option -> string option -> Pdfio.input -> Pdf.t

(** Same as [pdf_of_input], but from an OCaml channel. *)
val pdf_of_channel : ?revision:int -> ?source:string -> string option -> string option -> in_channel -> Pdf.t

(** As [pdf_of_channel], but delay loading of streams and parsing of objects like [pdf_of_input_lazy]. *)
val pdf_of_channel_lazy : ?revision:int -> ?source:string -> string option -> string option -> in_channel -> Pdf.t

(** Read a PDF from the given filename with optional user and owner passwords. *)
val pdf_of_file : ?revision:int -> string option -> string option -> string -> Pdf.t

(** {2 Configuration and debugging} *)

(** If set, some debug output is produced. *)
val read_debug : bool ref

(** If set, a malformed PDF will cause an error, not an attempt to read using
    the malformed PDF reader. *)
val error_on_malformed : bool ref

(** If set, we always use the malformed PDF reader. For debug. *)
val debug_always_treat_malformed : bool ref

(** {2 Low level functions} *)

(** Read the number of revisions of the document, by performing a dummy read. For
example, if this function returns 3, then appropriate values to pass to
[?revision] in a subsequent call to pdf_of_input are 1, 2, and 3. *)
val revisions : Pdfio.input -> int

(** Return encryption method in use *)
val what_encryption : Pdf.t -> Pdfwrite.encryption_method option

(** Return list of permissions *)
val permissions : Pdf.t -> Pdfcrypt.permission list

(** Given a filename, see if the file is linearized. *)
val is_linearized : Pdfio.input -> bool

(** Read a PDF header *)
val read_header : Pdfio.input -> int * int

(* Read characters until a PDF delimiter. *)
val getuntil_white_or_delimiter : Pdfio.input -> char list

(* Read characters until a PDF delimiter, returned as a string. *)
val getuntil_white_or_delimiter_string : Pdfio.input -> string

(** Read characters until a predicate is true. If the boolean is set, end of
    input is considered a delimiter. *)
val getuntil : bool -> (char -> bool) -> Pdfio.input -> char list

(** Throw away characters until a predicate is true. If the boolean is set, end
    of input is considered a delimiter. *)
val ignoreuntil : bool -> (char -> bool) -> Pdfio.input -> unit

(** Drop whitespace characters from an input. *)
val dropwhite : Pdfio.input -> unit

(** Lex a name, assuming there is one to lex. *)
val lex_name : Pdfio.input -> Pdfgenlex.t

(** Lex a number, assuming there is one to lex. *)
val lex_number : Pdfio.input -> Pdfgenlex.t

(** Lex a string, assuming there is one to lex. *)
val lex_string : Pdfio.input -> Pdfgenlex.t

(** Lex a hexadecimal string, assuming there is one to lex. *)
val lex_hexstring : Pdfio.input -> Pdfgenlex.t

(** Lex a comment, assuming there is one to lex. *)
val lex_comment : Pdfio.input -> Pdfgenlex.t

(** Lex a dictinonary, assuming there is one to lex. *)
val lex_dictionary : bool -> Pdfio.input -> Pdfgenlex.t list

(** Lex stream data of the given length. If the boolean is true, then actually
    read the data. If not, merely record the intention to. *)
val lex_stream_data : Pdfio.input -> int -> bool -> Pdfgenlex.t

(** Parse a PDF object. If [failure_is_ok] is set, a null object with high
    object number is returned, instead of an exception being raised. *)
val parse : ?failure_is_ok:bool -> Pdfgenlex.t list -> int * Pdf.pdfobject

(** Parse a single object. *)
val parse_single_object : string -> Pdf.pdfobject

(** String representation of a lexeme *) 
val string_of_lexeme : Pdfgenlex.t -> string

(** Print a lexeme to Standard Output with a space after it, for debug. *)
val print_lexeme : Pdfgenlex.t -> unit

(**/**)

val endpage : (Pdf.t -> int) ref
