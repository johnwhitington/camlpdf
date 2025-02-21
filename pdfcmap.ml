open Pdfutil
open Pdfio

type cmap =
  {map : (int * string) list;
   wmode : int option;
   usecmap : string option;
   supplement : int option}

(* Parse a /ToUnicode CMap to extract font mapping. *)
type section =
  | BfChar of char list
  | BfRange of char list

let rec getuntilend prev = function
  | [] -> rev prev, []
  | 'e'::'n'::'d'::'b'::'f'::'c'::'h'::'a'::'r'::more -> rev prev, more
  | h::t -> getuntilend (h::prev) t

let rec getuntilend_range prev = function
  | [] -> rev prev, []
  | 'e'::'n'::'d'::'b'::'f'::'r'::'a'::'n'::'g'::'e'::more -> rev prev, more
  | h::t -> getuntilend_range (h::prev) t

let rec get_section = function
  | [] -> None
  | 'b'::'e'::'g'::'i'::'n'::'b'::'f'::'c'::'h'::'a'::'r'::more ->
      let numbers, rest = getuntilend [] more in
        Some (BfChar numbers, rest)
  | 'b'::'e'::'g'::'i'::'n'::'b'::'f'::'r'::'a'::'n'::'g'::'e'::more ->
      let numbers, rest = getuntilend_range [] more in
        Some (BfRange numbers, rest)
  | _::t -> get_section t

(* Read a character code. *)
let rec read_number = function
  | x::more when Pdf.is_whitespace x -> read_number more
  | '<'::a::'>'::more ->
      int_of_string (implode ['0'; 'x'; a]), more
  | '<'::a::b::'>'::more ->
      int_of_string (implode ['0'; 'x'; a; b]), more
  | '<'::a::b::c::'>'::more ->
      int_of_string (implode ['0'; 'x'; a; b; c]), more
  | '<'::a::b::c::d::'>'::more ->
      int_of_string (implode ['0'; 'x'; a; b; c; d]), more
  | [] -> raise Not_found
  | _ -> raise (Pdf.PDFError "Unknown number in /ToUnicode")

(* Read the bytes of the UTF-16BE unicode sequence as a string. *)
let fail () =
  raise (Pdf.PDFError "Bad unicode value")

let rec read_unicode = function
  | x::rest when Pdf.is_whitespace x -> read_unicode rest
  | '<'::rest ->
      let chars, rest  = cleavewhile (neq '>') rest in
        let is_hex_digit = function
          | x when (x >= '0' && x <= '9') || (x >= 'a' && x <= 'f') || (x >= 'A' && x <= 'F') -> true
          | _ -> false
        in
          iter
            (fun x -> if not (is_hex_digit x) then fail ())
            chars;
          if length chars > 0 && even (length chars) then
            let bytes =
              map
                (function
                  | [x; y] -> char_of_int (int_of_string (implode ['0'; 'x'; x; y]))
                  | _ -> assert false)
                (splitinto 2 chars)
            in
              let rest' =
                match rest with
                | [] -> []
                | _ -> tl rest
              in
                implode bytes, rest'
          else
            fail ()
  | _ -> fail ()

let rec get_sections chars =
  match get_section chars with
  | None -> []
  | Some (sec, restchars) ->
      sec::get_sections restchars

let pairs_of_section = function
  | BfChar numbers ->
      let results = ref []
      in let numbers = ref numbers in
        begin try
          while true do
            let number, rest = read_number !numbers in
              let str, rest = read_unicode rest in
                numbers := rest;
                results =| (number, str)
          done;
          []
        with
          Not_found -> rev !results
        end
  | BfRange numbers ->
      let results = ref []
      in let numbers = ref numbers in
        begin try
          while true do
            let src1, rest  = read_number !numbers in
              let src2, rest  = read_number rest in
                if src1 > src2 then raise (Pdf.PDFError "Bad /ToUnicode") else
                  match rest with
                  | '<'::_ ->
                      (* It's a single unicode string *)
                      let increment_final code d =
                        match code with
                        | "" -> ""
                        | s ->
                            let chars = rev (explode s) in
                              implode ((rev (tl chars)) @
                              [char_of_int (int_of_char (hd chars) + d)])
                      in
                        let code, rest = read_unicode rest in
                          results =@
                            rev
                              (combine
                                (ilist src1 src2)
                                (map (increment_final code) (ilist 0 (src2 - src1))));
                          numbers := rest
                  | '['::rest ->
                      (* It's several. *)
                      let rest = ref rest in
                        results =@
                          combine
                            (ilist src1 src2)
                            (map
                              (fun _ ->
                                 let num, rest' = read_unicode !rest in
                                   rest := rest';
                                   num)
                              (ilist 0 (src2 - src1)));
                      rest := (match !rest with [] -> [] | x -> tl x);
                      numbers := !rest
                  | _ -> raise (Pdf.PDFError "Bad BfRange")
          done;
          []
        with
          Not_found -> rev !results
        end

let extract_specifics data =
  let wmode = ref None in
  let usecmap = ref None in
  let supplement = ref None in
  let read_number t =
    let h, t = cleavewhile isdigit t in
      int_of_string (implode h), t
  in
  let read_reversed_name t =
    let h, t = cleavewhile (neq '/') t in
      implode ('/'::rev h), t
  in
  let rec find = function
    | [] -> ()
    | '/'::'S'::'u'::'p'::'p'::'l'::'e'::'m'::'e'::'n'::'t'::' '::t ->
        let n, t = read_number t in
          supplement := Some n;
          find t
    | '/'::'W'::'M'::'o'::'d'::'e'::' '::t ->
        let n, t = read_number t in
          wmode := Some n;
          find t
    | 'p'::'a'::'m'::'c'::'e'::'s'::'U'::' '::t ->
        let n, t = read_reversed_name t in
          usecmap := Some n;
          find t
    | h::t -> find t
  in
    let chars = charlist_of_bytes data in
      find chars;
      find (rev chars);
      Printf.printf "WMode: %s\n" (match !wmode with None -> "None" | Some n -> string_of_int n);
      Printf.printf "Usecmap: %s\n" (match !usecmap with None -> "None" | Some n -> n);
      Printf.printf "Supplement: %s\n" (match !supplement with None -> "None" | Some n -> string_of_int n);
      (!wmode, !usecmap, !supplement)

let rec parse_cmap pdf cmap =
  match cmap with
  | Pdf.Stream {contents = (dict, Pdf.Got data)} ->
      Pdfcodec.decode_pdfstream pdf cmap;
      begin match cmap with
      | Pdf.Stream {contents = (dict, Pdf.Got data)} ->
          let wmode, usecmap, supplement = extract_specifics data in
          begin try
            {map =
               flatten
                 (map pairs_of_section
                   (get_sections
                      (lose Pdf.is_whitespace (charlist_of_bytes data))));
            wmode;
            usecmap;
            supplement}
          with
            e ->
              Pdfe.log (Printf.sprintf "/ToUnicode Parse Error : %s\n" (Printexc.to_string e));
              raise e
          end
      | _ -> assert false
      end
  | Pdf.Stream {contents = (_, Pdf.ToGet _)} ->
      Pdf.getstream cmap;
      parse_cmap pdf cmap
  | e -> raise (Pdf.PDFError (Printf.sprintf "Bad /ToUnicode %s" (Pdfwrite.string_of_pdf e)))
