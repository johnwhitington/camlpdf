(* JPEG images *)
open Pdfutil
open Pdfutil
open Pdfio

(* This function is used by the inline image code to return the JPEG data
without decoding it, placing the stream pointer at the byte following the data.
This is done by searching for the two-byte sequence &FF, &D9 (the end of image
marker). This sequence cannot occur in the entropy-encoded image data because
the encoder always puts a &00 after any &FF. *)
let get_jpeg_data i =
  let s, data = input_output_of_bytes 4096 in
    let fin = ref false and last = ref 0 in
    while not !fin do
      match i.input_byte () with
      | x when x = no_more ->
          raise (Failure "Could not read JPEG data - end of stream")
      | 0xD9 ->
          if !last = 0xFF then set fin else last := 0xD9;
          s.output_byte 0xD9
      | n -> last := n; s.output_byte n
    done;
    extract_bytes_from_input_output s data

(* Return the width and height of a JPEG image, per Michael Petrov's C version. *)
exception Answer of int * int

let jpeg_dimensions bs =
  try
    let get = bget bs in
    let i = ref 0 in
    if get !i = 0xFF && get (!i + 1) = 0xD8 && get (!i + 2) = 0xFF && get (!i + 3) = 0xE0 then
      begin
        i += 4;
        if
           get (!i + 2) = int_of_char 'J' && get (!i + 3) = int_of_char 'F'
        && get (!i + 4) = int_of_char 'I' && get (!i + 5) = int_of_char 'F'
        && get (!i + 6) = 0
        then
          let block_length = ref (get !i * 256 + get (!i + 1)) in
          while !i < bytes_size bs do
            i := !i + !block_length;
            if !i > bytes_size bs then raise (Pdf.PDFError "jpeg_dimensions: too short") else
            if get !i <> 0xFF then raise (Pdf.PDFError "jpeg_dimensions: not a valid block") else
            if get (!i + 1) = 0xC0 then
              raise (Answer ((get (!i + 5) * 256 + get (!i + 6), get (!i + 7) * 256 + get (!i + 8))))
            else
             begin
               i += 2;
               block_length := get !i * 256 + get (!i + 1)
             end
          done
        else
          raise (Pdf.PDFError "jpeg_dimensions: Not a valid JFIF string")
      end
    else
      raise (Pdf.PDFError "jpeg_dimensions: Not a valid SOI header");
    (0, 0)
 with
   Answer (w, h) -> (w, h)
