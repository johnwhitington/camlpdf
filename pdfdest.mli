(** Destinations *)

(** The target of a destination is either a page object in the same PDF (given
by object number of page object), or a page number in an external file. *)
type targetpage =
  | PageObject of int
  | OtherDocPageNumber of int

(** Destinations (and actions) *)
type t =
  | Action of Pdf.pdfobject
  | NullDestination
  | NamedDestination of string
  | StringDestination of string
  | XYZ of targetpage * float option * float option * float option
  | Fit of targetpage
  | FitH of targetpage * float option
  | FitV of targetpage * float option
  | FitR of targetpage * float * float * float * float
  | FitB of targetpage
  | FitBH of targetpage * float option
  | FitBV of targetpage * float option

(** Read a destination given a PDF and destination object. If [shallow] is
    true, actions will be kept in PDF form, rather than followed to their
    destinations.  *)
val read_destination : ?shallow:bool -> Pdf.t -> Pdf.pdfobject -> t

(** Write a destination to a [Pdf.pdfobject]. *)
val pdfobject_of_destination : t -> Pdf.pdfobject

(** Transform a destination by a matrix *)
val transform_destination : Pdf.t -> Pdftransform.transform_matrix -> t -> t
