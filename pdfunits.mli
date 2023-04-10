(** Units and Unit Conversion *)

(** The type of units *)
type t = PdfPoint | Inch | Centimetre | Millimetre

(** Convert a measurement to points *)
val points : float -> t -> float
