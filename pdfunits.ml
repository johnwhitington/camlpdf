type t = PdfPoint | Inch | Centimetre | Millimetre

let points x = function
  | PdfPoint -> x
  | Inch -> x *. 72.
  | Centimetre -> x *. 28.3465
  | Millimetre -> x *. 2.83465

let inches x = function
  | Inch -> x
  | PdfPoint -> x /. 72.
  | Centimetre -> x /. 2.54
  | Millimetre -> x /. 25.4

let centimetres x = function
  | Centimetre -> x
  | Inch -> x *. 2.54
  | Millimetre -> x /. 10.
  | PdfPoint -> x /. 28.3456

let millimetres x u =
  centimetres x u *. 10.
