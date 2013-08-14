(** Standard PDF Fonts *)

(** Calculate the width, in millipoints, of a string in the given font, taking
into account kerning if the first argument is true. *)
val textwidth : bool -> Pdftext.standard_font -> string -> int

(** The appropriate amount to subtract from the y-coordinate of a 1pt text line
to place it vertically centered around the y coordinate, rather than with the
baseline at that y coordinate. *)
val baseline_adjustment : Pdftext.standard_font -> int

