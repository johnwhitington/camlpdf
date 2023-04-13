(** Units and Unit Conversion *)

(** The type of units *)
type t = PdfPoint | Inch | Centimetre | Millimetre

(** Convert a measurement to points *)
val points : float -> t -> float

(** Convert a measurement to inches *)
val inches : float -> t -> float

(** Convert a measurement to centimetres *)
val centimetres : float -> t -> float

(** Convert a measurement to millimetres *)
val millimetres : float -> t -> float
