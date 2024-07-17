(** Structure trees *)

(** Trim structure tree to remove parts marked as not in the page range given. *)
val trim_structure_tree : Pdf.t -> int list -> unit

(** Remove now-deleted items from the parent tree *)
(*val postprocess_parent_tree : Pdf.t -> unit*)

val renumber_parent_trees : Pdf.t list -> unit

(** Merge the structure hierarchy *)
val merge_structure_hierarchy : Pdf.t -> Pdf.t list -> int option

(** / **)
val endpage : (Pdf.t -> int) ref
