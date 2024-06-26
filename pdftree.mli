val read_name_tree : Pdf.t -> Pdf.pdfobject -> (string * Pdf.pdfobject) list

val read_number_tree : Pdf.t -> Pdf.pdfobject -> (string * Pdf.pdfobject) list

val build_name_tree : bool -> Pdf.t -> (string * Pdf.pdfobject) list -> Pdf.pdfobject
