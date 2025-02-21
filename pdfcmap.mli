type cmap =
  {map : (int * string) list;
   wmode : int option;
   usecmap : string option;
   supplement : int option}

(** Parse a [/ToUnicode] entry. *)
val parse_tounicode : Pdf.t -> Pdf.pdfobject -> cmap
