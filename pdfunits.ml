type t = PdfPoint | Inch | Centimetre | Millimetre

let points x = function
  | PdfPoint -> x
  | Inch -> x /. 72.
  | Centimetre -> x /. 72. *. 2.54
  | Millimetre -> x /. 72. *. 2.54 *. 10.
