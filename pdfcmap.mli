(** Parsing ToUnicode and other CMaps *)

type t =
  {map : (int * string) list;
   wmode : int}

(** Parse a CMap. *)
val parse_cmap : Pdf.t -> Pdf.pdfobject -> t
