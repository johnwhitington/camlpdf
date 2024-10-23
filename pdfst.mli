(** Structure trees *)

(** Trim structure tree to remove parts marked as not in the page range given. *)
val trim_structure_tree : Pdf.t -> int list -> unit

(** Renumber parent trees in a list of PDFs so as not to clash when
    subsequently merged. *)
val renumber_parent_trees : Pdf.t list -> unit

(** Merge the structure tree given the part-merged PDF and list of original
    PDFs. Returns the object number of the new structure tree, if any. If
    ?add_toplevel_document is true (default false) a PDF/UA-2-style top-level
    /Document will be added to the structure tree. *)
val merge_structure_trees : ?add_toplevel_document:bool -> Pdf.t -> Pdf.t list -> int option

(**/**)
val endpage : (Pdf.t -> int) ref
