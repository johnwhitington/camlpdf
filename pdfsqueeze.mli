(** Lossless compression of PDF files *)

(** Squeeze a PDF. Currently this performs the following operations:
  a) Coalesce duplicate streams *)

val squeeze : Pdf.t -> unit

