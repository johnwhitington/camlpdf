type t = PdfPoint | Inch | Centimetre | Millimetre

let points x = function
  | PdfPoint -> x
  | Inch -> x *. 72.
  | Centimetre -> x *. 28.3465
  | Millimetre -> x *. 2.83465
