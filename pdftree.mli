(** Name and Number Trees *)
 
(** Subject to API changes. *)

(** Read a name tree as a flat (key, value) list *)
val read_name_tree : Pdf.t -> Pdf.pdfobject -> (string * Pdf.pdfobject) list

(** Read a number tree as a flat (key, value) list but with the numbers as strings. *)
val read_number_tree : Pdf.t -> Pdf.pdfobject -> (string * Pdf.pdfobject) list

(** Build a name or number tree (bool: true if number, false otherwise) from
    flat (key, value) list *)
val build_name_tree : bool -> Pdf.t -> (string * Pdf.pdfobject) list -> Pdf.pdfobject
