(** Page-level functionality *)

(** {2 Page rotations} *)

(** The type of the four rotations of pages. This defines how a viewing
application (e.g Acrobat) displays the page. *)
type rotation =
  Rotate0 | Rotate90 | Rotate180 | Rotate270

(** Utility function to convert from rotation to integers. *)
val int_of_rotation : rotation -> int

(** The reverse. raises [Pdf.PDFError] if its input modulo 360 is not 0, 90, 180
or 270. *)
val rotation_of_int : int -> rotation

(** {2 Pages} *)

(** A type representing a page. [content] is the list of objects containing the
graphical content stream (see the [Pdfpages] module), [mediabox] the page size,
[resources] the page's resource dictionary, [rotate] its rotation and [rest] any
other entries to reside in the page dictionary. *)
type t =
  {content : Pdf.pdfobject list;
   mediabox : Pdf.pdfobject;
   resources : Pdf.pdfobject;
   rotate : rotation;
   rest : Pdf.pdfobject}

(** Create a page with empty content, media box from the given paper size, empty
resources, zero rotation and no extra dictionary entries. *)
val blankpage : Pdfpaper.t -> t

(** The same, but given a page size rectangle. *)
val custompage : Pdf.pdfobject -> t

(** Extract the page tree from a PDF document and parse it to a list of page
objects. Owing to the [rest] entry in the [page] type, no information is lost.
*)
val pages_of_pagetree : Pdf.t -> t list

(** Build a page tree from a list of pages and install it in the given PDF
document. The resultant document and the number of the new page root object are
returned. If the document already contains a page root, it is overwritten but is
not garbage collected. *)
val add_pagetree : t list -> Pdf.t -> Pdf.t * int

(** Given the page root number (for instance that returned by [add_pagetree]),
any specific extra dictionary entries and a PDF document, build a document root.
Returns the new document. If a root exists, it is overwritten but is not garbage
collected. *)
val add_root : int -> (string * Pdf.pdfobject) list -> Pdf.t -> Pdf.t

(** Number of pages in a document, faster than reading the pages and counting. *)
val endpage : Pdf.t -> int

(** Find a page indirect from the page tree of a document, given a page number. *)
val page_object_number : Pdf.t -> int -> int option

(** {2 Compound operations} *)

(** Rename the resources within a number of page resource dictionaries and
contents, so as to allow them to be merged without name clashes. *)
val renumber_pages : Pdf.t -> t list -> t list

(** Change the pages in a document for some new ones. If the boolean is true
and the number of pages in the old and new documents are equal, references to
the old pages from outside the page tree (for instance in destinations or
bookmarks) are renumbered. This ensures bookmarks are preserved correctly. A
list of (page number, matrix) pairs may also be supplied if the boolean is true
and the number of old and new pages are equal. This allows transformed pages
(e.g scaled) to have their bookmark destionations pointed at correctly. *)
val change_pages :
  ?matrices:(int * Pdftransform.transform_matrix) list ->
  ?changes:((int * int) list) -> bool -> Pdf.t -> t list -> Pdf.t

(** Return a pdf with a subset of pages, but nothing else changed - exactly the
same page object numbers, so bookmarks etc still work. Also sorts out bookmarks
so only those in the range are kept. *)
val pdf_of_pages : ?retain_numbering:bool -> Pdf.t -> int list -> Pdf.t

(**/**)

val shortest_unused_prefix : Pdf.t -> string

val add_prefix : Pdf.t -> string -> unit

val protect : Pdf.t -> Pdf.pdfobject -> Pdf.pdfobject list -> Pdfops.t list

val prepend_operators : Pdf.t -> Pdfops.t list -> ?fast:bool -> t -> t

val postpend_operators : Pdf.t -> Pdfops.t list -> ?fast:bool -> t -> t

val pagenumber_of_target : ?fastrefnums:(int, int) Hashtbl.t -> Pdf.t -> Pdfdest.t -> int

val target_of_pagenumber : Pdf.t -> int -> Pdfdest.t

val flat_pagetrees : bool ref
