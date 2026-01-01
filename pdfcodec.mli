(** Encoding and Decoding PDF Streams *)

(**
{b Currently supported:}
- Decoders: ASCIIHexDecode, ASCII85Decode, FlateDecode, RunLengthDecode, CCITTFaxDecode, LZWDecode, JBIG2 (via jbig2dec)
- Encoders: ASCIIHexDecode, ASCII85Decode, FlateDecode, RunLengthDecode, CCITTFaxDecode G3, CCITTFaxDecode G4 (via imagemagick)
- Decode predictors: PNG (all), TIFF (8-bit only).
*)

(** {2 Types} *)

(** Supported encodings. *)
type encoding =
  | ASCIIHex
  | ASCII85
  | RunLength
  | Flate
  | CCITT of int * int
  | CCITTG4 of int * int

(** Predictors. *)
type predictor =
    TIFF2
  | PNGNone
  | PNGSub
  | PNGUp
  | PNGAverage
  | PNGPaeth
  | PNGOptimum

(** There was bad data. *)
exception Couldn'tDecodeStream of string

(** CamlPDF doesn't support this encoding or its predictor. *)
exception DecodeNotSupported of string

(** {2 Encoding} *)

(** Encode a PDF stream with an encoding. [im] is the path to imagemagick (only
    for CCITTG4) If [only_if_smaller] is true, the stream remains uncompressed
    if compression would make it bigger. [predictor] and [predictor_columns]
    describe any predictor to be used. *)
val encode_pdfstream : Pdf.t -> encoding -> ?im:string -> ?only_if_smaller:bool -> ?predictor:predictor -> ?predictor_columns:int -> Pdf.pdfobject -> unit

(** {2 Decoding} *)

(** Given a document and stream, decode. The pdf document is updated
with the decoded stream. May raise either of the exceptions above. If
[jbig2dec] is given, JBIG2 streams will be decompressed with it. Otherwise,
they are left alone. *)
val decode_pdfstream : ?jbig2dec:string -> Pdf.t -> Pdf.pdfobject -> unit

(** Given a document and stream decode just one stage. May raise either of the
exceptions above. If [jbig2dec] is given, JBIG2 streams will be decompressed
with it. Otherwise, they are left alone. *)
val decode_pdfstream_onestage : ?jbig2dec:string -> Pdf.t -> Pdf.pdfobject -> unit

(** Given a document and stream decode until there's an unknown decoder. May
raise [Couldn'tDecodeStream]. If [jbig2dec] is given, JBIG2 streams will be
decompressed with it. Otherwise, they are left alone. *)
val decode_pdfstream_until_unknown : ?jbig2dec:string -> Pdf.t -> Pdf.pdfobject -> unit

(** Given a [Pdfio.input] with pointer at the first byte and an inline image
stream dictionary, decode the first decoder and its predictor. Return the data,
or [None] if this decoder isn't supported but the data pointer has been left in
the right place. The exceptions above can both be raised, in the case of bad
data or a completely unknown encoding. *)
val decode_from_input : Pdfio.input -> Pdf.pdfobject -> Pdfio.bytes option

(** {2 Low level functionality} *)

(** Setting this changes globally the FlateDecode compression level. Default 6. *)
val flate_level : int ref

(** Encode data in FlateDecode. *)
val encode_flate : Pdfio.bytes -> Pdfio.bytes

(** Decode data in FlateDecode. *)
val decode_flate : Pdfio.bytes -> Pdfio.bytes

(** Encode data in CCITTDecode Group 3. *)
val encode_ccitt : int -> int -> Pdfio.bytes -> Pdfio.bytes

(** Encode data in CCITTDecode Group 4. Presently we do this via imagemagick, given by [im]. *)
val encode_ccittg4 : ?im:string -> int -> int -> Pdfio.bytes -> Pdfio.bytes

(** Setting this boolean prints some debug information. *)
val debug : bool ref

(**/**)

(* Inter-module recursion. *)
val string_of_pdf : (Pdf.pdfobject -> string) ref

val pdf_of_file : (?revision:int -> string option -> string option -> string -> Pdf.t) ref

val encode_predictor : int -> int -> int -> int -> Pdfio.bytes -> Pdfio.bytes

val decode_predictor : int -> int -> int -> int -> Pdfio.bytes -> Pdfio.bytes
