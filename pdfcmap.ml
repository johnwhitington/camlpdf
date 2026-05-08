open Pdfutil
open Pdfio

type t =
  {map : (int * string) list;
   wmode : int}

(* Parse a CMap to extract font mapping. *)
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

let pairs_of_section ~to_unicode = function
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
                if src1 > src2 then raise (Pdf.PDFError "Bad CMap src1 > src2") else
                  match rest with
                  | '<'::_ ->
                      let increment_final code d =
                        if to_unicode then
                          (* If a /ToUnicode, it's a single unicode string, the last byte of which should be incremented. *)
                          match code with
                          | "" -> ""
                          | s ->
                              let chars = rev (explode s) in
                                (* If increment is too large, result is undefined. *)
                                try implode ((rev (tl chars)) @ [char_of_int (int_of_char (hd chars) + d)]) with _ -> s
                        else
                          (* If an ordinary CMap, it's just a hex number, and increment is unbounded. *)
                          let n = int_of_string ("0x" ^ code) in
                            Printf.sprintf "%04x" (n + d)
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

let extract_wmode data =
  let rec find = function
    | [] -> 0
    | '/'::'W'::'M'::'o'::'d'::'e'::' '::'0'::_ -> 0
    | '/'::'W'::'M'::'o'::'d'::'e'::' '::'1'::_ -> 1
    | h::t -> find t
  in
    find (charlist_of_bytes data)

let rec parse_cmap ~to_unicode pdf cmap =
  let cmap = Pdf.direct pdf cmap in
  match cmap with
  | Pdf.Stream {contents = (dict, Pdf.Got data)} ->
      Pdfcodec.decode_pdfstream pdf cmap;
      begin match cmap with
      | Pdf.Stream {contents = (dict, Pdf.Got data)} ->
          let wmode =
            match Pdf.lookup_direct pdf "/WMode" dict with
            | Some (Pdf.Integer w) -> w
            | _ -> extract_wmode data
          in
            begin try
              {map =
                 flatten
                   (map (pairs_of_section ~to_unicode)
                     (get_sections
                        (lose Pdf.is_whitespace (charlist_of_bytes data))));
              wmode}
            with
              e ->
                Pdfe.log (Printf.sprintf "CMap Parse Error : %s\n" (Printexc.to_string e));
                raise e
            end
      | _ -> assert false
      end
  | Pdf.Stream {contents = (_, Pdf.ToGet _)} ->
      Pdf.getstream cmap;
      parse_cmap ~to_unicode pdf cmap
  | Pdf.Name "/Identity-H" ->
      {map = map (fun charcode -> (charcode, Printf.sprintf "%04X" charcode)) (ilist 0 65536);
       wmode = 0}
  | Pdf.Name "/Identity-V" ->
      {map = map (fun charcode -> (charcode, Printf.sprintf "%04X" charcode)) (ilist 0 65536);
       wmode = 1}
  | e -> raise (Pdf.PDFError (Printf.sprintf "Unknown CMap %s" (Pdfwrite.string_of_pdf e)))
