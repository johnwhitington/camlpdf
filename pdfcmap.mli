(** Parsing ToUnicode and other CMaps *)

type cmap =
  {map : (int * string) list;
   wmode : int option;
   usecmap : string option;
   supplement : int option}

(** Parse a CMap. *)
val parse_cmap : Pdf.t -> Pdf.pdfobject -> cmap
