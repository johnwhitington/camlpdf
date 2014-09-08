(** Parse Adobe Font Metrics files *)

(** Return the header, character metrics (character, width) and kerning
pairs (char1, char2, kerm) from an AFM file. May raise [Failure]. *)
val read : Pdfio.input -> (string * string) list * (int * int) list * (int * int * int) list

