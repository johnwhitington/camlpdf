(** Parse Adobe Font Metrics files *)

(** Return the character metrics (character, width) and kerning pairs (char1,
char2, kerm) from an AFM file. May raise [Failure]. *)
val read : Pdfio.input -> (int * int) list * (int * int * int) list

