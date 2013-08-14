(* The 14 Standard PDF Fonts (Widths and Kerns). *)
open Pdfutil

let read_afm afm =
  let ws, ks = Pdfafm.read (Pdfio.input_of_string afm) in
    hashtable_of_dictionary ws,
    hashtable_of_dictionary (map (fun (c, c', k) -> (c, c'), k) ks)

(* Main functions *)
let tables =
  [Pdftext.TimesRoman, memoize (fun () -> read_afm Pdfafmdata.times_roman_afm);
   Pdftext.TimesBold, memoize (fun () -> read_afm Pdfafmdata.times_bold_afm);
   Pdftext.TimesItalic, memoize (fun () -> read_afm Pdfafmdata.times_italic_afm);
   Pdftext.TimesBoldItalic, memoize (fun () -> read_afm Pdfafmdata.times_bold_italic_afm);
   Pdftext.Helvetica, memoize (fun () -> read_afm Pdfafmdata.helvetica_afm);
   Pdftext.HelveticaBold, memoize (fun () -> read_afm Pdfafmdata.helvetica_bold_afm);
   Pdftext.HelveticaOblique, memoize (fun () -> read_afm Pdfafmdata.helvetica_oblique_afm);
   Pdftext.HelveticaBoldOblique, memoize (fun () -> read_afm Pdfafmdata.helvetica_bold_oblique_afm);
   Pdftext.Courier, memoize (fun () -> read_afm Pdfafmdata.courier_afm);
   Pdftext.CourierBold, memoize (fun () -> read_afm Pdfafmdata.courier_bold_afm);
   Pdftext.CourierOblique, memoize (fun () -> read_afm Pdfafmdata.courier_oblique_afm);
   Pdftext.CourierBoldOblique, memoize (fun () -> read_afm Pdfafmdata.courier_bold_oblique_afm);
   Pdftext.Symbol, memoize (fun () -> read_afm Pdfafmdata.symbol_afm);
   Pdftext.ZapfDingbats, memoize (fun () -> read_afm Pdfafmdata.zapf_dingbats_afm)]

(* The height of a capital H divided by 2. Allows the text to be placed vertically aligned with its middle rather than baseline *)
let baseline_adjustment = function
  | Pdftext.TimesRoman -> 662 / 2
  | Pdftext.TimesBold -> 676 / 2
  | Pdftext.TimesItalic -> 653 / 2
  | Pdftext.TimesBoldItalic -> 669 / 2
  | Pdftext.Helvetica -> 718 / 2
  | Pdftext.HelveticaBold -> 718 / 2
  | Pdftext.HelveticaOblique -> 718 / 2
  | Pdftext.HelveticaBoldOblique -> 718 / 2
  | Pdftext.Courier -> 562 / 2
  | Pdftext.CourierBold -> 562 / 2
  | Pdftext.CourierOblique -> 562 / 2
  | Pdftext.CourierBoldOblique -> 562 / 2
  | Pdftext.Symbol -> 673 / 2 (* Based on left paren, not H, since no CapHeight in afm file. *)
  | Pdftext.ZapfDingbats -> 705 / 2 (* Based on majority of characters, not H, since no CapHeight in afm file, *)

(* Calculate the width of a list of characters, taking account of kerning. *)
let find_kern kerns key =
  match tryfind kerns key with Some x -> x | None -> 0

let find_width widths h =
  match tryfind widths h with Some x -> x | None -> 0

let rec width dokern widths kerns = function
  | [] -> 0
  | [h] -> find_width widths h
  | h::h'::t ->
      find_width widths h +
      (if dokern then find_kern kerns (h, h') else 0) +
      width dokern widths kerns (h'::t)

(* The main function. Give a font and the text string. *)
let textwidth dokern f s =
  let widths, kerns = lookup_failnull f tables () in
    width dokern widths kerns (map int_of_char (explode s))

