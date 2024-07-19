(** Structure trees *)

(** Trim structure tree to remove parts marked as not in the page range given. *)
val trim_structure_tree : Pdf.t -> int list -> unit

(** Renumber parent trees in a list of PDFs so as not to clash when
    subsequently merged. *)
val renumber_parent_trees : Pdf.t list -> unit

(** Merge the structure tree given the part-merged PDF and list of original
    PDFs. Returns the object number of the new structure tree, if any. *)
val merge_structure_trees : Pdf.t -> Pdf.t list -> int option

(**/**)
val endpage : (Pdf.t -> int) ref
