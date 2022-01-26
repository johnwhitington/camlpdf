(** A very fast lexer for very basic tokens *)

(** To avoid too much storage allocation (and hence garbage collection), we use
the same data type for this very basic lexing module as for the main lexing in
[Pdfread]. Eventually, we may unify this with the parsing type too. *)
type t =
  | LexNull
  | LexBool of bool
  | LexInt of int
  | LexReal of float
  | LexString of string
  | LexName of string
  | LexLeftSquare
  | LexRightSquare
  | LexLeftDict
  | LexRightDict
  | LexStream of Pdf.stream
  | LexEndStream
  | LexObj
  | LexEndObj
  | LexR
  | LexComment
  | StopLexing
  | LexNone

(** For debug only. *)
val string_of_token : t -> string

(** For debug only. *)
val string_of_tokens : t list -> string

(** Lex a single token from a [Pdfio.input]. *)
val lex_single : (Pdfio.input -> t)

(** Lex all the token in a [Pdfio.input]. *)
val lex : (Pdfio.input -> t list)

(** Lex all the tokens from a string. *)
val lex_string : string -> t list
