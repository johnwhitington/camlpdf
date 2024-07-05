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

(** Number of pages in a document, simply believing top level /Count *)
val endpage_fast : Pdf.t -> int

(** Find a page indirect from the page tree of a document, given a page number. *)
val page_object_number : Pdf.t -> int -> int option

(** {2 Compound operations} *)

(** Rename the resources within a number of page resource dictionaries and
contents, so as to allow them to be merged without name clashes. *)
val renumber_pages : Pdf.t -> t list -> t list

(** Change the pages in a document for some new ones. If the boolean is true
and the number of pages in the old and new documents are equal (or ~changes, a
list of (from, to) page nummber changes, is provided), references to the old
pages from outside the page tree (for instance in destinations or bookmarks)
are renumbered. This ensures bookmarks are preserved correctly. A list of (page
number, matrix) pairs may also be supplied if the boolean is true and the
number of old and new pages are equal. This allows transformed pages (e.g
scaled) to have their bookmark destionations pointed at correctly. *)
val change_pages :
  ?matrices:(int * Pdftransform.transform_matrix) list ->
  ?changes:((int * int) list) -> bool -> Pdf.t -> t list -> Pdf.t

(** Return a pdf with a subset of pages, but nothing else changed - exactly the
same page object numbers, so bookmarks etc still work. Also sorts out bookmarks
so only those in the range are kept. *)
val pdf_of_pages : ?retain_numbering:bool -> ?process_struct_tree:bool -> Pdf.t -> int list -> Pdf.t

(** {2 Miscellaneous} *)

(** Make a PDF rectangle from a Paper.papersize. *)
val rectangle_of_paper : Pdfpaper.t -> Pdf.pdfobject

(** Find the shortest lower-case alphabetic string which is not a prefix of any
    name in /Resources. This prefix can be added to the other PDF's names, and
    will never clash with any of these. *)
val shortest_unused_prefix : Pdf.t -> string

(** For every page in the PDF, add the prefix to any name in /Resources and add
    the prefix to any name used in any content streams. *)
val add_prefix : Pdf.t -> string -> unit

(** Calling [protect pdf] add stack operators to a pre-ISO
    content stream to ensure it is composeable. *)
val protect : Pdfops.t list -> Pdfops.t list

(** Add operators to the beginning of a page. If [fast] is set (default false),
    don't check for mismatched stack operators. *)
val prepend_operators : Pdf.t -> Pdfops.t list -> ?fast:bool -> t -> t

(** Add operators to the end of a page. If [fast] is set (default false),
    don't check for mismatched stack operators. *)
val postpend_operators : Pdf.t -> Pdfops.t list -> ?fast:bool -> t -> t

(** Return a page number given a destination. Supply [fastrefnums] from a
    previous call to [Pdf.page_reference_numbers] to speed things up. *)
val pagenumber_of_target : ?fastrefnums:(int, int) Hashtbl.t -> Pdf.t -> Pdfdest.t -> int

(** Build a basic [Fit] destintation from a page number of a PDF. *)
val target_of_pagenumber : Pdf.t -> int -> Pdfdest.t

(** Generate a PDF file with a single A4 page. Unlike [Pdf.empty] it is a fully
    valid PDF. *)
val minimum_valid_pdf : unit -> Pdf.t

(**/**)

val process_xobjects : Pdf.t ->
           t ->
           (Pdf.t ->
            Pdf.pdfobject -> Pdf.pdfobject list -> Pdf.pdfobject list) ->
           unit

val combine_pdf_resources : Pdf.t -> Pdf.pdfobject -> Pdf.pdfobject -> Pdf.pdfobject

val ppstub : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c * 'a * Pdftransform.transform_matrix
