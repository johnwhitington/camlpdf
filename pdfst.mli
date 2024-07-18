(** Structure trees *)

(** Trim structure tree to remove parts marked as not in the page range given. *)
val trim_structure_tree : Pdf.t -> int list -> unit

(** Renumber parent trees so as not to clash when merged. *)
val renumber_parent_trees : Pdf.t list -> unit

(** Merge the structure hierarchy *)
val merge_structure_hierarchy : Pdf.t -> Pdf.t list -> int option

(** / **)
val endpage : (Pdf.t -> int) ref
