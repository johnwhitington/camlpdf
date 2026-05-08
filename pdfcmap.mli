(** Parsing ToUnicode and other CMaps *)

type t =
  {map : (int * string) list;
   wmode : int}

(** Parse a CMap. *)
val parse_cmap : to_unicode:bool -> Pdf.t -> Pdf.pdfobject -> t
