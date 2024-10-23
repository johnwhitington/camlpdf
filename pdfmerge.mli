(** Merge PDF files *)

(** Merge PDF files. [merge_pdfs retain_numbering remove_duplicate_fonts
    ?process_struct_trees ?add_toplevel_document names pdfs ranges] will merge
    the given PDFs. if [retain_numbering] is true, page labels are retained. If
    [remove_duplicate_fonts] is true, duplicate fonts are detected and
    coalesced. If [process_struct_trees] is true (default false), structure
    trees are trimmed and merged. If [add_toplevel_document] is also true
    (default false) a PDF/UA-2-style top level /Document structure tree entry
    is added. [names] is a list of strings the same length as the list of
    PDFs. Equal names imply equal PDFs (for efficiency).  [ranges] is a list
    of page ranges. A page range is a list of pages to select, in order. For
    example [[1]] or [[2; 2; 2; 5; 6; 7]]. *)
val merge_pdfs :
  bool -> bool -> ?process_struct_trees:bool -> ?add_toplevel_document:bool -> string list -> Pdf.t list -> int list list -> Pdf.t

(** Remove duplicate fonts from a PDF. For example, if it was created by merging
    several documents from the same source. *)
val remove_duplicate_fonts : Pdf.t -> unit
