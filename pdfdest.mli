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
  | NamedDestinationElsewhere of string
  | XYZ of targetpage * float option * float option * float option
  | Fit of targetpage
  | FitH of targetpage * float option
  | FitV of targetpage * float option
  | FitR of targetpage * float * float * float * float
  | FitB of targetpage
  | FitBH of targetpage * float option
  | FitBV of targetpage * float option

(** Read a destination given a PDF and destionation object. *)
val read_destination : Pdf.t -> Pdf.pdfobject -> t

(** Write a destination to a [Pdf.pdfobject]. *)
val pdfobject_of_destination : t -> Pdf.pdfobject

(**/**)
val string_of_destination : t -> string

