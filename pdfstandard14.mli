(** Standard PDF Fonts *)

(** Calculate the width, in millipoints, of a string in the given font, taking
into account kerning if the first argument is true. *)
val textwidth : bool -> Pdftext.standard_font -> string -> int

(** The appropriate amount to subtract from the y-coordinate of a 1pt text line
to place it vertically centered around the y coordinate, rather than with the
baseline at that y coordinate. *)
val baseline_adjustment : Pdftext.standard_font -> int

(** The data extracted from the font AFM. This is a triple, consisting of a
table of header pairs, a table of (character number, width) pairs and a table
of (first, second, kern) triples representing the kerning table. *)
val afm_data :
  Pdftext.standard_font ->
    (string, string) Hashtbl.t * (int, int) Hashtbl.t * (int * int, int) Hashtbl.t

(** Return a suitable StemV value for a standard font *)
val stemv_of_standard_font : Pdftext.standard_font -> int

(** Return a suitable flags value for a standard font *)
val flags_of_standard_font : Pdftext.standard_font -> int

