(* Compress the text in a file, with OCaml lexical conventions. *)
let load f =
  let ch = open_in_bin f in
    let len = in_channel_length ch in
    let str = String.make len ' ' in
      really_input ch str 0 len;
      close_in ch;
      str

let save f s =
  let ch = open_out_bin f in
    output_string ch s;
    close_out ch

let compress data outfile =
  save outfile @@
  String.escaped @@
  Pdfio.string_of_bytes @@
  Pdfcodec.encode_flate @@
  Pdfio.bytes_of_string data

let _ =
  match Sys.argv with
    [|_; infile; outfile|] -> compress (load infile) outfile
  | _ -> Printf.eprintf "Syntax: compress <in> <out>"
       
