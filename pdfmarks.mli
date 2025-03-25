(** Bookmarks *)

(** The type of bookmarks. *)
type t =
  {level : int;
   text : string;
   target : Pdfdest.t;
   isopen : bool;
   colour : float * float * float;
   flags : int}

(** Debug string from a bookmark. *)
val string_of_bookmark : t -> string

(** Read the bookmarks from a document. If [preserve_actions] is set, the
    [target] preserves actions rather than extracting the resultant rectangle.
    See discussion of [shallow] in [Pdfdest]. *)
val read_bookmarks : preserve_actions:bool -> Pdf.t -> t list

(** Remove the bookmarks from a document. *)
val remove_bookmarks : Pdf.t -> Pdf.t

(** Add bookmarks to a document, replacing any currently there. *)
val add_bookmarks : t list -> Pdf.t -> Pdf.t

(** Transform a bookmark's destination *)
val transform_bookmark : Pdf.t -> Pdftransform.transform_matrix -> t -> t
