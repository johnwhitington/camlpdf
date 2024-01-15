(** Extract Images *)

type pixel_layout =
  | BPP1 (* Black and white *)
  | BPP8 (* Greyscale *)
  | BPP24 (* Colour *)
  | BPP48 (* 48 bit colour *)

type t =
  | JPEG of Pdfio.bytes * float list option
  | JPEG2000 of Pdfio.bytes * float list option
  | JBIG2 of Pdfio.bytes * float list option * int option
  | Raw of int * int * pixel_layout * Pdfio.bytes

(** Given a pdf document, resources dictionary and a stream representing an
image, return a triple : width, height, and a stream of (width * height * 3)
bytes RGBRGB etc. In all instances, if JPEG or JPEG2000 or JBIG2 is the compression
 method, data is returned still encoded. *)
val get_image_24bpp :
  Pdf.t -> Pdf.pdfobject -> Pdf.pdfobject -> t

(** Given a PDF, an image and a /Resources dictionary, return the colourspace
of the image *)
val colspace : Pdf.t -> Pdf.pdfobject -> Pdf.pdfobject -> Pdfspace.t

(** Given a PDF and an image, return any /BPC entry *)
val bpc : Pdf.t -> Pdf.pdfobject -> Pdf.pdfobject option
