open Pdfutil
open Pdfio

let write_debug = ref false

(* The file header. We include four larger-than-127 bytes as requested by the
standard to help FTP programs distinguish binary/text transfer modes. *)
let header pdf =
  "%PDF-" ^ string_of_int pdf.Pdf.major ^ "." ^ string_of_int pdf.Pdf.minor ^ "\n%\128\129\130\131\n"

(* Build a cross-reference table string. *)
let pad_to_ten ch s =
  let l = String.length s in
    if l > 10 then raise (Pdf.PDFError "xref too big") else
      let t = String.make 10 ch in
        String.blit s 0 t (10 - l) l;
        t

let string_of_xref n =
  pad_to_ten '0' (string_of_int n) ^ " 00000 n \n" 

(* Write the cross-reference table to a channel. *)
let write_xrefs xrefs i =
  i.output_string "xref\n";
  i.output_string ("0 " ^ string_of_int (length xrefs + 1) ^ " \n");
  i.output_string "0000000000 65535 f \n";
  iter (function x -> i.output_string (string_of_xref x)) xrefs

(* Convert a string to one suitable for output. The function [escape] escapes
parentheses and backslashes. *)
let b = Buffer.create 30

let make_pdf_string s =
  Buffer.clear b;
  Buffer.add_char b '(';
  String.iter
    (function
       | ('(' | ')' | '\\') as c -> Buffer.add_char b '\\'; Buffer.add_char b c
       | '\n' -> Buffer.add_char b '\\'; Buffer.add_char b 'n'
       | '\r' -> Buffer.add_char b '\\'; Buffer.add_char b 'r'
       | '\t' -> Buffer.add_char b '\\'; Buffer.add_char b 't'
       | '\b' -> Buffer.add_char b '\\'; Buffer.add_char b 'b'
       | '\012' -> Buffer.add_char b '\\'; Buffer.add_char b 'f'
       | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b ')';
  Buffer.contents b

(* We have two kinds of flat data to write: Strings and streams (we cannot
represent streams as strings, since there is a langauge limit on the length of
strings. *)
type writeout =
  | WString of string
  | WStream of Pdf.stream

(* We want real numbers with no exponents (format compliance), and no trailing
zeroes (compactness). (Jan 2012 - have added back in special case for whole
numbers. Can still get trailing zeroes on small values e.g 0.00001 => 0.000010,
but no printf way to prevent this). *)
let format_real x =
  let fl = floor x in
    if fl = x then string_of_int (int_of_float fl) else
      if x < 0.0001 && x > -. 0.0001 then Printf.sprintf "%f" x else string_of_float x

(* Character codes in a name < 33 or > 126 are replaced with hashed combinations
(e.g #20 for space). If the name contains the null character, an exception is
raised. *)
let hexchar = function
  | 0 -> '0' | 1 -> '1' | 2 -> '2' | 3 -> '3' | 4 -> '4' | 5 -> '5' | 6 -> '6' | 7 -> '7'
  | 8 -> '8' | 9 -> '9' | 10 -> 'A' | 11 -> 'B' | 12 -> 'C' | 13 -> 'D' | 14 -> 'E' | 15 -> 'F'
  | _ -> raise (Failure "hexchar")

let make_pdf_name_inner b s =
  for x = 1 to String.length s - 1 do (* skip / *)
    match String.get s x with
    | '\000' ->
      raise (Pdf.PDFError "Name cannot contain the null character")
    | h when h < '\033' || h > '\126' || Pdf.is_delimiter h || h = '#' ->
      Buffer.add_char b '#';
      Buffer.add_char b (hexchar ((int_of_char h) / 16));
      Buffer.add_char b (hexchar ((int_of_char h) mod 16));
    | h ->
      Buffer.add_char b h
  done

(* See if a name needs altering by [make_pdf_name_inner]. We ignore the first
character, since a '/' is a delimter, and this is fine... *)
let rec needs_processing_inner s p l =
  (p <= l - 1) &&
    (match s.[p] with
    | '\000' -> raise (Pdf.PDFError "Name cannot contain the null character")
    | x when x < '\033' || x > '\126' || Pdf.is_delimiter x || x = '#' -> true
    | _ -> needs_processing_inner s (p + 1) l)

let needs_processing s =
  let l = String.length s in
    if l < 2 then false else
      needs_processing_inner s 1 l

let b = Buffer.create 30

let make_pdf_name n =
  if needs_processing n then
    if n = "" || String.get n 0 <> '/' then raise (Pdf.PDFError "bad name") else
      begin
        Buffer.clear b;
        Buffer.add_char b '/';
        make_pdf_name_inner b n;
        Buffer.contents b
      end
  else
    n

(* Calculate a strings and streams representing the given pdf datatype instance,
assuming it has no unresolved indirect references. *)
let rec strings_of_array f changetable = function
  | [] -> ()
  | [x] -> strings_of_pdf f changetable x
  | h::(h'::_ as tail) ->
      strings_of_pdf f changetable h;
      begin match h' with
        Pdf.Name _ | Pdf.String _ | Pdf.Array _ | Pdf.Dictionary _ -> ()
      | _ ->
          match h with
            Pdf.String _ | Pdf.Array _ | Pdf.Dictionary _ -> ()
          | _ -> f (WString " ")
      end;
      strings_of_array f changetable tail

and strings_of_dictionary f changetable = function
  | [] -> ()
  | [(k, v)] ->
      f (WString (make_pdf_name k));
      begin match v with
        Pdf.Name _ | Pdf.String _ | Pdf.Array _ | Pdf.Dictionary _ -> ()
      | _ -> f (WString " ")
      end;
      strings_of_pdf f changetable v
  | (k, v)::t ->
      f (WString (make_pdf_name k));
      begin match v with
        Pdf.Name _ | Pdf.String _ | Pdf.Array _ | Pdf.Dictionary _ -> ()
      | _ -> f (WString " ")
      end;
      strings_of_pdf f changetable v;
      strings_of_dictionary f changetable t

and strings_of_pdf f changetable = function
  | Pdf.Null ->  f (WString "null")
  | Pdf.Boolean b -> f (WString (string_of_bool b))
  | Pdf.Integer n ->  f (WString (string_of_int n))
  | Pdf.Real r -> f (WString (format_real r))
  | Pdf.String s -> f (WString (make_pdf_string s))
  | Pdf.Name n -> f (WString (make_pdf_name n))
  | Pdf.Array elts ->
      f (WString "[");
      strings_of_array f changetable elts;
      f (WString "]");
  | Pdf.Dictionary entries ->
      f (WString "<<");
      strings_of_dictionary f changetable entries;
      f (WString ">>");
  | Pdf.Stream {contents = (dict, data)} ->
      strings_of_pdf f changetable dict;
      f (WString "\010stream\010");
      f (WStream data);
      f (WString "\010endstream");
  | Pdf.Indirect n ->
      let n' =
        try Hashtbl.find changetable n with Not_found -> n
      in
        f (WString (string_of_int n'));
        f (WString " 0 R")

let strings_of_pdf_return obj =
  let strings = ref [] in
    strings_of_pdf (function x -> strings := x::!strings) (Hashtbl.create 0) obj;
    rev !strings

(* Produce a single string from a PDF object. Only use for things which will
always fall under the string size limit. *)
let b = Buffer.create 100

let string_of_pdf s =
  Buffer.clear b;
  strings_of_pdf (function (WString x) -> Buffer.add_string b x |  _ -> ()) (Hashtbl.create 0) s;
  Buffer.contents b

(* Inter-module recursion. *)
let _ = Pdfcrypt.string_of_pdf := string_of_pdf
let _ = Pdfcodec.string_of_pdf := string_of_pdf

let debug_whole_pdf pdf =
  Printf.printf "trailerdict = %s\n" (string_of_pdf pdf.Pdf.trailerdict);
  Pdf.objiter (fun i o -> Printf.printf "%i = %s\n" i (string_of_pdf o)) pdf

(* Calculate strings, one for each indirect object in the body. *)
let strings_of_object (n, pdfobject) =
  let strings = ref [] in
  strings := [WString (string_of_int n ^ " 0 obj\n")];
  strings_of_pdf (function x -> strings := x::!strings) (Hashtbl.create 0) pdfobject;
  strings := WString "\nendobj\n"::!strings;
  rev !strings

let strings_of_pdf_object f (_, pdfobject) n' changetable =
  f (WString (string_of_int n' ^ " 0 obj\n"));
  strings_of_pdf f changetable pdfobject;
  f (WString "\nendobj\n")

(* Output a stream. *)
let output_stream o s =
  Pdf.getstream s;
  match s with
  | Pdf.Stream {contents = _, Pdf.Got arr} ->
      if bytes_size arr > 0 then
        getinit o arr 0 (bytes_size arr)
  | _ -> raise (Pdf.PDFError "output_stream")

(* Encrypting a PDF while writing *)
type encryption_method =
  | PDF40bit
  | PDF128bit
  | AES128bit of bool (* true = encrypt metadata, false = don't. *)
  | AES256bit of bool (* as above *)
  | AES256bitISO of bool (* as above *)
  | AlreadyEncrypted (* Used as a flag to prevent garbage collection *)

type encryption = 
  {encryption_method : encryption_method;
   owner_password : string;
   user_password : string;
   permissions : Pdfcrypt.permission list}

let crypt_if_necessary pdf = function
  | None -> pdf
  | Some enc ->
      let f =
        match enc.encryption_method with
        | PDF40bit -> Pdfcrypt.encrypt_pdf_40bit
        | PDF128bit -> Pdfcrypt.encrypt_pdf_128bit
        | AES128bit em -> Pdfcrypt.encrypt_pdf_AES em
        | AES256bit em -> Pdfcrypt.encrypt_pdf_AES256 em
        | AES256bitISO em -> Pdfcrypt.encrypt_pdf_AES256ISO em
        | AlreadyEncrypted -> fun _ _ _ pdf -> pdf
      in
        f enc.user_password enc.owner_password enc.permissions pdf

(* Linearized (Fast Web View) writing *)

(* Find all the indirect numbers reachable from an entry in a dictionary,
including the indirect of that dictionary entry, if it's an indirect. *)
let reference_numbers_of_dict_entry pdf dict entry =
  match dict with
  | Pdf.Dictionary d ->
      begin match lookup entry d with
      | Some x ->
          Pdf.objects_referenced
            ["/Thumb"] [("/Type", Pdf.Name "/Page"); ("/Type", Pdf.Name "/Pages")] pdf x
      | None -> []
      end
  | _ ->
      raise (Pdf.PDFError "reference_numbers_of_dict_entry: not a dictionary")

(* The Part 6 (First Page Section) object numbers.
(1) Page object for first page
(2) Outline hierarchy, if PageMode is UseOutlines
(3) All objects the page object refers to, except page nodes or other page objects *)
let part6_parts_of_pdf pdf =
  let catalog = Pdf.catalog_of_pdf pdf in
    let first_page_objnum =
      match Pdf.page_reference_numbers pdf with
      | [] -> raise (Pdf.PDFError "No pages in document")
      | i::_ -> i
    in
      let outline_objnums =
        match Pdf.lookup_direct pdf "/PageMode" catalog with
        | Some (Pdf.Name "/UseOutlines") ->
            reference_numbers_of_dict_entry pdf catalog "/Outlines"
        | _ -> []
      in
        let referenced_from_page =
          Pdf.objects_referenced
            ["/Thumb"] [("/Type", Pdf.Name "/Page"); ("/Type", Pdf.Name "/Pages")]
            pdf (Pdf.lookup_obj pdf first_page_objnum)
        in
          setify_preserving_order (first_page_objnum :: outline_objnums @ referenced_from_page)

(* The Part 4 (Catalog and Document-Level Objects) object numbers. *)
let part4_parts_of_pdf pdf =
  let catalog_num =
    match pdf.Pdf.trailerdict with
    | Pdf.Dictionary d ->
        begin match lookup "/Root" d with
        | Some (Pdf.Indirect i) -> i
        | _ -> raise (Pdf.PDFError "Bad catalog")
        end
    | _ -> raise (Pdf.PDFError "Bad catalog")
  in     
    let catalog = Pdf.catalog_of_pdf pdf in
      let indirects_from_catalog no_follow_entries no_follow_contains entry =
        match catalog with
        | Pdf.Dictionary d ->
            begin match lookup entry d with
            | Some v ->
                Pdf.objects_referenced no_follow_entries no_follow_contains pdf v
            | _ -> []
            end
        | _ -> raise (Pdf.PDFError "bad catalog")
      in
        let sources_follow =
          ["/ViewerPreferences"; "/PageMode"; "/Threads"; "/OpenAction"; "/Encrypt"]
        in
          let objnum_of_acroform =
            match catalog with
            | Pdf.Dictionary d ->
                begin match lookup "/AcroForm" d with
                | Some (Pdf.Indirect i) -> [i]
                | _ -> []
                end
            | _ -> []
          in
            (* Catalog number is the head. *)
            setify_preserving_order
              (catalog_num::
                flatten
                  (map (indirects_from_catalog
                  ["/Parent"]
                  ["/Type", Pdf.Name "/Page"; "/Type", Pdf.Name "/Pages"]) sources_follow) @
                  objnum_of_acroform)

(* Part 7: For each page, objects reachable from this page which are reachable
   from no others;
   Part 8: Objects referenced from > 1 page;
   Part 9: Anything else  *)
let print_nums ls =
  iter (Printf.printf "%i ") ls;
  flprint "\n"

let get_main_parts p3nums pdf =
  let objects_left = setminus (Pdf.objnumbers pdf) p3nums
  in let pagenums = 
    match Pdf.page_reference_numbers pdf with
    | [] -> raise (Pdf.PDFError "This PDF has no pages")
    | _::t -> t
  in
    let pages = map (Pdf.lookup_obj pdf) pagenums in
      let objects_from_each_page =
        map
          (Pdf.objects_referenced
             ["/Thumb"; "/Parent"]
             [("/Type", Pdf.Name "/Page"); ("/Type", Pdf.Name "/Pages")]
             pdf)
          pages
      in
        if !write_debug then
          begin
            Printf.printf "objects from each page...\n";
            iter
              (fun (n, objs) -> Printf.printf "\nPAGE %i " n; print_nums objs)
                (combine (indx0 objects_from_each_page) objects_from_each_page)
          end;
        let histogram =
          collate compare (sort compare (flatten objects_from_each_page))
        in
          if !write_debug then
            begin
              Printf.printf "get main parts: histogram:\n";
              iter print_nums histogram;
            end;
          let shared_objects =
            flatten
              (map (function x -> [hd x])
                (keep (function [] | [_] -> false | _ -> true) histogram))
          in
            let shared_objects = setminus shared_objects p3nums in
              if !write_debug then
                begin
                  Printf.printf "shared objects...\n";
                  print_nums shared_objects;
                  iter
                    (function n ->
                       Printf.printf "Object %i:\n" n;
                       flprint (string_of_pdf (Pdf.lookup_obj pdf n));
                       flprint "\n")
                    shared_objects;
                end;
              let unshared_lists =
                map (lose (mem' shared_objects)) objects_from_each_page
              in
                (* Put them in order (page object first) and flatten *)
                let part7_pages =
                  map2 (fun p l -> p::lose (eq p) l) pagenums unshared_lists
                in
                  let unshared_objects = flatten part7_pages in
                    let unshared_objects = setminus_preserving_order unshared_objects p3nums in
                      let part9 =
                        setminus (setminus objects_left shared_objects) unshared_objects
                      in
                        if !write_debug then
                          begin
                            flprint "p7nums on output from get_main_parts:\n";
                            iter
                              (function n ->
                                 Printf.printf "Object %i:\n" n;
                                 flprint (string_of_pdf (Pdf.lookup_obj pdf n));
                                 flprint "\n")
                              unshared_objects;
                          end;
                         part7_pages, unshared_objects, shared_objects, part9

(* We output 10-character blanks XXXXXXXXXX, overwriting them when we know the
values, at the end of the process. *)

(* Return all trailerdict entries except for size, root and prev, as a partial
dictionary entry list represented as a string. Number changes will need to have
occured for everything in the trailerdict by now, since we're creating X O R
references to them...*)
let rest_of_trailerdict_entries pdf =
  let str =
    string_of_pdf
      (fold_left Pdf.remove_dict_entry pdf.Pdf.trailerdict ["/Prev"; "/Size"; "/Root"])
  in
    implode (rev (tl (tl (rev (tl (tl (explode str)))))))

let flatten_W o = function
 | WString s -> o.output_string s
 | WStream data -> output_stream o (Pdf.Stream {contents = Pdf.Null, data})

(* Renumber old numbers to new ones, renumbering any other objects in the PDF
which clash. Returns the new PDF. *)
let lin_changes old_nums new_nums pdf =
  assert (length old_nums = length new_nums);
  if old_nums = [] then hashtable_of_dictionary [] else
    let clash_changes =
      let maxnum = pdf.Pdf.objects.Pdf.maxobjnum + 1 in
        let new_objnums = ilist maxnum (maxnum + length new_nums - 1) in
          combine new_nums new_objnums
    in
      let changes = clash_changes @ combine old_nums new_nums in
        hashtable_of_dictionary changes

let lin_renumber old_nums new_nums pdf =
  assert (length old_nums = length new_nums);
  match new_nums with
  | [] -> pdf
  | _ -> Pdf.renumber (lin_changes old_nums new_nums pdf) pdf

(* Rember the items in [l] according to the (parital) changes given. *)
let list_renumber old_nums new_nums pdf l =
  let changes = lin_changes old_nums new_nums pdf in
    map (fun x -> match tryfind changes x with Some y -> y | None -> x) l

(* List of (object number, final position of object in file) pairs *)
type xrefblank =
  | PDFObj of int
  | LinearizationDictionaryPosition
  | PrimaryHintStreamPosition
  | FileLength
  | HintOffset
  | HintLength
  | EndOfFirstPage
  | MainXRefTableFirstEntry
  | Prev

(* Replace the markers with the (now calculated) contents *)
let replace_xs o object_positions x_positions specials =
  iter
    (function
     | PDFObj i, xpos ->
         begin match lookup i !object_positions with
         | Some pos ->
             o.seek_out xpos;
             o.output_string (pad_to_ten '0' (string_of_int pos))
         | None -> raise (Pdf.PDFError "Linearization inconsistency")
         end
     | other, xpos ->
         let pad =
           match other with
           | LinearizationDictionaryPosition
           | PrimaryHintStreamPosition -> '0'
           | _ -> ' '
         in
           match lookup other !specials with
           | Some pos ->
               o.seek_out xpos;
               o.output_string (pad_to_ten pad (string_of_int pos))
           | _ -> ())
    !x_positions 

(* Outputting specials markers *)
let output_special_xref_line o xrefblank x_positions =
  x_positions =| (xrefblank, o.pos_out ());
  o.output_string "XXXXXXXXXX 00000 n \n"

let output_xref_line o x_positions objnum =
  output_special_xref_line o (PDFObj objnum) x_positions

let output_special o xrefblank x_positions =
  x_positions =| (xrefblank, o.pos_out ());
  o.output_string "XXXXXXXXXX"

(* The minimum number of bits needed to represent the number given. *)
let bits_needed n =
  if n = 0 then 0 else log2of (pow2lt n * 2)

(* The number of bytes which an object will use up in the file. *)
let object_bytes pdf objnum =
  let strings = strings_of_object (objnum, Pdf.lookup_obj pdf objnum)
  in let length_of_string = function
    | WString s -> String.length s
    | WStream (Pdf.Got data) -> bytes_size data
    | WStream (Pdf.ToGet (_, _, length)) -> length
  in
    fold_left ( + ) 0 (map length_of_string strings) 

(* Same for list of objects *)
let objects_bytes pdf objs =
  fold_left ( + ) 0 (map (object_bytes pdf) objs)

(* Calculates a bitstream representing the page offset hint table. *)
let page_offset_hint_table pdf pages first_page_objects shared_objects object_positions =
  assert (length pages > 0);
  let objects_reachable_from_each_page =
    let referenced page_objnum =
      Pdf.objects_referenced
        [] [("/Type", Pdf.Name "/Page"); ("/Type", Pdf.Name "/Pages")]
        pdf (Pdf.lookup_obj pdf page_objnum)
    in
      map
        (function p -> keep (mem' shared_objects) (setify (referenced <| hd p)))
        pages
  in
    if !write_debug then
      begin
        Printf.printf "In page_offset_hint_table, we have %i pages\n" (length pages);
        let pp = ref 0 in
        iter
          (fun objlist ->
            Printf.printf "Page %i\n" !pp;
            incr pp;
            iter
              (function o ->
                Printf.printf "%i: " o;
                flprint (string_of_pdf (Pdf.lookup_obj pdf o));
                flprint "\n")
            objlist)
          pages;
        Printf.printf "End of page_offset_hint_table printer";
        flprint "\n";

        Printf.printf "There are %i first_page_objects...viz:\n" (length first_page_objects);
            iter
              (function o ->
                Printf.printf "%i: " o;
                flprint (string_of_pdf (Pdf.lookup_obj pdf o));
                flprint "\n")
            first_page_objects;
      end;
  (* Remove anything in first page from the pages, so lengths are correct *)
  let pages =
     hd pages::map (lose (mem' first_page_objects)) (tl pages)
  in
  let page_lengths = map length pages
  in let page_byte_lengths = map (objects_bytes pdf) pages in
  let least_in_page = hd (sort compare page_lengths)
  in let most_in_page = hd (sort rev_compare page_lengths)
  in let least_bytes_in_page = hd (sort compare page_byte_lengths)
  in let most_bytes_in_page = hd (sort rev_compare page_byte_lengths) in
  (* Least number of objects in a page *)
  let item1 = least_in_page
  (* Location of first page's page object *)
  in let item2 = lookup_failnull (hd (hd pages)) !object_positions
  (* Number of bits needed to represent the difference between the greatest and
  least number of objects in a page *)
  in let item3 = bits_needed (most_in_page - least_in_page) 
  (* Least length of a page in the file in bytes *)
  in let item4 = least_bytes_in_page
  (* Number of bits needed to represent the difference between the greatest and
  least length of a page in the file in bytes *)
  in let item5 = bits_needed (most_bytes_in_page - least_bytes_in_page)
  (* Number of bits needed to represent the greatest number of shared object
  references. (in other words, in part 8) *)
  in let item9 = item5 
  in let item10 =
    bits_needed (hd (sort rev_compare
    (length (hd pages)::map length objects_reachable_from_each_page)))
  (* Number of bits needed to represent the numerically greatest shared object
  identifier used by the pages *)
  in let item11 = bits_needed (max 0 (length shared_objects + length first_page_objects - 1))
  (* Number of bits needed to represent the numerator of the fractional position
  for each shared object reference. *)
  in let item12 = 0 (* CHANGED *)
  (* The denominator of the fractional position for each shared object
  reference. *)
  in let item13 = 1
  in let b = make_write_bitstream () in
    if !write_debug then
      begin
        Printf.printf "%i %i %i %i %i %i %i %i %i %i %i %i %i\n"
        item1 item2 item3 item4 item5 0 0 0 item5 item10 item11 item12 item13;
      end;
    (* Write the header *)
    putval b 32 (i32ofi item1);
    putval b 32 (i32ofi item2);
    putval b 16 (i32ofi item3);
    putval b 32 (i32ofi item4);
    putval b 16 (i32ofi item5);
    putval b 32 0l;
    putval b 16 0l;
    putval b 32 (i32ofi item4); (* q.v. qpdf *)
    putval b 16 (i32ofi item9);
    putval b 16 (i32ofi item10);
    putval b 16 (i32ofi item11);
    putval b 16 (i32ofi item12);
    putval b 16 (i32ofi item13);
    (* Now the per-page entries *)
    (* Items 1 *)
    for x = 1 to length pages do
      putval b item3 (i32ofi (length (select x pages) - item1))
    done;
    Pdfio.align_write b;
    (* Item 2 *)
    for x = 1 to length pages do
      putval b item5 (i32ofi (select x page_byte_lengths - item4))
    done;
    Pdfio.align_write b;
    (* Item 3 *)
    for x = 1 to length pages do
      if x = 1 then
        (*if length pages > 1
          then*) putval b item10 0l
          (*else putval b item10 (i32ofi (length (hd pages)))*)
      else
        putval b item10 (i32ofi (length (select x objects_reachable_from_each_page)))
    done;
    Pdfio.align_write b;
    (* Item 4 *)
    for x = 1 to length pages do
      if x = 1 (*&& length pages > 1*) then () else
        let shared_objects_reachable =
          select x objects_reachable_from_each_page
        in
          let table =
            let all_objs = first_page_objects @ shared_objects in
              hashtable_of_dictionary (combine all_objs (indx all_objs))
          in
            iter
              (fun s ->
                putval b item11 (i32ofi (Hashtbl.find table s)))
              shared_objects_reachable
    done;
    Pdfio.align_write b;
    (* Item 5 *)
    for x = 1 to length pages do
      if x = 1 (*&& length pages > 1*) then () else
        let shared_objects_reachable =
          select x objects_reachable_from_each_page
        in
          for y = 1 to length shared_objects_reachable do
            putval b item12 0l (* Always use 0 / 1 fraction *)
          done
    done;
    Pdfio.align_write b;
    (* Item 7 (No item 6) *)
    for x = 1 to length pages do
      putval b item9 0l (* Ignored *)
    done;
    b

(* Shared object hint table *)
let shared_object_hint_table pdf first_page_objects shared_objects shared_object_positions =
  assert (length shared_objects = length shared_object_positions);
  if !write_debug then
    begin
      Printf.printf "Making shared_object_hint_table: %i shared objects:\n" (length shared_objects);
      iter (Printf.printf "%i ") shared_objects;
      flprint "\n";
      Printf.printf "%i first page ones:\n" (length first_page_objects);
      iter (Printf.printf "%i ") first_page_objects;
      flprint "\n";
      Printf.printf "%i Shared object positions\n" (length shared_object_positions);
      iter (Printf.printf "%i ") first_page_objects;
      flprint "\n"
    end;
  (*i let lookup_pos = combine shared_objects shared_object_positions in i*)
  let lengths_of_shared_objects =
    map (object_bytes pdf) (shared_objects @ first_page_objects)
  in
    let least =
      match sort compare lengths_of_shared_objects with [] -> 0 | h::_ -> h
    and greatest =
      match sort rev_compare lengths_of_shared_objects with [] -> 0 | h::_ -> h
    in
      let b = make_write_bitstream () in
        (* Object number of first object in shared objects section *)
        let item1 = match shared_objects with [] -> 0 | h::_ -> h
        (* Location of the first object in the shared objects section *)
        in let item2 = 0 (*i match shared_objects with [] -> 0 | h::_ -> lookup_failnull h lookup_pos i*)
        (* The number of shared object entries for the first page (including unshared objects *)
        in let item3 = length first_page_objects
        in let item4 = length first_page_objects + length shared_objects
        (* The least length of a shared object group in bytes (= least length of an
        object in bytes) *)
        in let item6 = least
        (* Number of bits required to encode the difference between the greatest and
        smallest length of an shared object group (=object) in bytes *)
        in let item7 = bits_needed (greatest - least)
        in
          putval b 32 (i32ofi item1);
          putval b 32 (i32ofi item2);
          putval b 32 (i32ofi item3);
          putval b 32 (i32ofi item4);
          putval b 16 0l;
          putval b 32 (i32ofi item6);
          putval b 16 (i32ofi item7);
          (* Item 1s (byte lengths) *)
          iter
            (fun x ->
              let len = object_bytes pdf x - item6 in 
                putval b item7 (i32ofi len))
            (first_page_objects @ shared_objects);
          Pdfio.align_write b;
          (* flags - no signatures *)
          for x = 0 to item4 - 1 do putval b 1 0l done;
          Pdfio.align_write b;
          (* Item 2s *)
          iter (function _ -> putval b 1 0l) (first_page_objects @ shared_objects);
          Pdfio.align_write b;
          (* Item 4s *)
          iter (function _ -> putval b 0 0l) (first_page_objects @ shared_objects);
          b
          
(* This is filled in by the Pdfdoc module at code-loading time. It remains
static thereafter. *)
let pagetree_make_explicit = ref ident

(* OBJECT NUMBERS:
1..n    Objects not related to the first page
n+1     Linearization dictionary
n+2     Catalog
n+3     First page's page object
n+4..m  Rest of first page and related content
m + 1   Primary hint stream. *)
let pdf_to_output_linearized encrypt pdf o =
  if !write_debug then Printf.printf "Beginning of pdf_to_output_linearized. There are %i objects\n" (Pdf.objcard pdf);
  let specials = ref []
  and object_positions = ref []
  and x_positions = ref [] in
  let pdf = !pagetree_make_explicit pdf in
  Pdf.remove_unreferenced pdf;
  if !write_debug then Printf.printf "Removed unreferenced: now have %i objects\n" (Pdf.objcard pdf);
  let writeobj pdf p =
    let obj = 
      try Pdf.lookup_obj pdf p with
        | Not_found -> Pdf.Null
    in
      object_positions =| (p, o.pos_out ()); 
      iter (flatten_W o) (strings_of_object (p, obj))
  in
  let p4objs = [hd (part4_parts_of_pdf pdf)] in
  (* TEMPORARY: Delete most of part 4! Soo they end up in part 9! So what we need to do is sort the part4/part6 ordering problem below out. *)
    if !write_debug then
      begin
        Printf.printf "part4_parts_of_pdf gave us %i objects\n" (length p4objs);
        iter
          (function o ->
             Printf.printf "%i: " o;
             flprint (string_of_pdf (Pdf.lookup_obj pdf o));
             flprint "\n")
        p4objs;
        flprint "\n"
      end;
    (* First object is catalog *)
  let p6objs = part6_parts_of_pdf pdf in
    (* First object is first page's page object number *)
    if !write_debug then
      begin
        Printf.printf "part6_parts_of_pdf gave us %i objects\n" (length p6objs);
        iter
          (function o ->
             Printf.printf "%i: " o;
             flprint (string_of_pdf (Pdf.lookup_obj pdf o));
             flprint "\n")
          p6objs;
        flprint "\n";
      end;
  if (not (length p4objs > 0 && length p6objs > 0)) then raise (Pdf.PDFError "Could not linearize file: malformed");
  (* 23rd Feb 2012 We may have elements in both p4objs and p6objs, which is a
  problem. Solution is to remove any elements from part6 anything which is also
  in part4. This means each object only appears once, and all the eleborate
  stuff thus works. *)
  let p6objs =
    keep (fun x -> not (mem x p4objs)) p6objs
  in
  let objects_in_rest_of_file =
    Pdf.objcard pdf - length p4objs - length p6objs
  in
  (* Part 1: Header *)
  o.output_string (header pdf);
  (* Part 2: Linearization parameter dictionary *)
  let lin_dict_obj_number = objects_in_rest_of_file + 1 in
  specials =| (LinearizationDictionaryPosition, o.pos_out ());
  o.output_string
(string_of_int lin_dict_obj_number ^ " 0 obj\n<< /Linearized 1\n/L ");
  output_special o FileLength x_positions;
  o.output_string "\n/H [ ";
  output_special o HintOffset x_positions;
  o.output_string " ";
  output_special o HintLength x_positions;
  o.output_string "]\n";
  o.output_string ("/O " ^ string_of_int (objects_in_rest_of_file + 3) ^ "\n");
  o.output_string "/E ";
  output_special o EndOfFirstPage x_positions;
  o.output_string
("\n/N " ^ (string_of_int (length (Pdf.page_reference_numbers pdf))) ^ "\n/T");
  output_special o MainXRefTableFirstEntry x_positions;
  o.output_string "\n>>\nendobj\n";
  (* Part 3: First page cross-reference table and trailer *)
  let p3length = length p4objs + length p6objs + 2 in
  let p3nums =
    if p3length = 0 then [] else
      ilist_null
        (objects_in_rest_of_file + 2)
        (objects_in_rest_of_file + 2 + length p4objs + length p6objs - 1)
  in
  let order = (hd p4objs::hd p6objs::tl p4objs @ tl p6objs) in
  let new_p6objs = list_renumber order p3nums pdf p6objs in 
  let pdf = lin_renumber order p3nums pdf in
  let p7_pages, p7nums, p8nums, p9nums = get_main_parts p3nums pdf in
  if !write_debug then
    begin
      Printf.printf "Part 7 objects\n";
        iter
          (function o ->
             Printf.printf "%i: " o;
             flprint (string_of_pdf (Pdf.lookup_obj pdf o));
             flprint "\n")
          p7nums;
        flprint "\n";
      Printf.printf "Part 8 objects\n";
        iter
          (function o ->
             Printf.printf "%i: " o;
             flprint (string_of_pdf (Pdf.lookup_obj pdf o));
             flprint "\n")
          p8nums;
        flprint "\n";
      Printf.printf "Part 9 objects\n";
        iter
          (function o ->
             Printf.printf "%i: " o;
             flprint (string_of_pdf (Pdf.lookup_obj pdf o));
             flprint "\n")
          p9nums;
        flprint "\n"
    end;
  let p7length = objects_bytes pdf p7nums in
  let p8lengths = map (object_bytes pdf) p8nums in
  let main_nums = p7nums @ p8nums @ p9nums in
  let new_main_nums =
    if length main_nums > 0 then ilist 1 (length main_nums) else []
  in
  let list_renumber = list_renumber main_nums new_main_nums pdf in
  let p7_pages = map list_renumber p7_pages in
  let new_p6objs = list_renumber new_p6objs
  in let new_p8nums = list_renumber p8nums in
  let pdf = lin_renumber main_nums new_main_nums pdf in
  let pdf = crypt_if_necessary pdf encrypt in
  (* FIXME: With encryption, stream lengths can change slightly, and therefore would be wrong *)
  let position_of_first_page_xref_table = o.pos_out () in
  o.output_string
("xref\n" ^ string_of_int (objects_in_rest_of_file + 1) ^
" " ^ string_of_int p3length ^ " \n");
  output_special_xref_line o LinearizationDictionaryPosition x_positions;
  iter (output_xref_line o x_positions) p3nums;
  output_special_xref_line o PrimaryHintStreamPosition x_positions;
  o.output_string
("trailer\n << /Size " ^ string_of_int (Pdf.objcard pdf + 3) ^ " /Prev ");
  output_special o Prev x_positions;
  o.output_string
(" /Root " ^ string_of_int (objects_in_rest_of_file + 2) ^
" 0 R " ^ rest_of_trailerdict_entries pdf ^ ">>\n" ^ "startxref\n0\n%%EOF\n");
  (* Part 4 and Part 6: Document-level and first page  *)
  iter (writeobj pdf) p3nums;
  specials =| (EndOfFirstPage, o.pos_out ());
  (* Part 5: Primary hint stream *)
  let all_pages = tl p3nums::p7_pages in
  let p8positions = cumulative_sum (p7length + o.pos_out ()) p8lengths in
  let offset_table = page_offset_hint_table pdf all_pages new_p6objs new_p8nums object_positions in
  let shared_table = shared_object_hint_table pdf new_p6objs new_p8nums p8positions in
  let stream_content =
    bytes_of_write_bitstream <|
    write_bitstream_append_aligned offset_table shared_table
  in
  let hintstream_dict =
    Pdf.Dictionary
      [("/Length", Pdf.Integer (bytes_size stream_content));
       ("/S", Pdf.Integer (bytes_size (bytes_of_write_bitstream offset_table)))]
  in
  let stream_wstrings =
    strings_of_pdf_return
      (Pdf.Stream (ref (hintstream_dict, Pdf.Got (stream_content))))
  in let hint_num = Pdf.objcard pdf + 2 in
  let hs_offset = o.pos_out () in
  specials =| (PrimaryHintStreamPosition, hs_offset);
  specials =| (HintOffset, hs_offset);
  o.output_string ((string_of_int hint_num) ^ " 0 obj\n");
  iter (flatten_W o) stream_wstrings;
  o.output_string "\nendobj\n";
  let hs_length = o.pos_out () - hs_offset in
  specials =| (HintLength, hs_length);
  (* Parts 7, 8 and 9: Remaining pages and other objects. *)
  iter (writeobj pdf) new_main_nums;
  (* Part 11: Main cross-reference table and trailer *)
  specials =| (Prev, o.pos_out ());
  let main_size = length p7nums + length p8nums + length p9nums + 1 in
  o.output_string ("xref\n0 " ^ string_of_int main_size ^ "\n");
  specials =| (MainXRefTableFirstEntry, o.pos_out ());
  o.output_string ("0000000000 65536 f \n");
  iter (output_xref_line o x_positions) new_main_nums;
  o.output_string ("trailer\n<< /Size " ^ string_of_int main_size ^ " >>\nstartxref\n");
  o.output_string (string_of_int position_of_first_page_xref_table);
  o.output_string "\n%%EOF\n";
  specials =| (FileLength, o.pos_out ());
  replace_xs o object_positions x_positions specials

(* Functions for object streams. NB no attempt is made to catch objects which
shouldn't be in a stream - this is the responsibility of the caller. *)
let bake_object_streams pdf numbers =
  iter
    (fun (tostream, objects) ->
       let data, first =
         let output, d = Pdfio.input_output_of_bytes 32000 in
           let strings =
             map (fun x -> string_of_pdf (Pdf.lookup_obj pdf x) ^ " ") objects
           in
             iter (Pdf.removeobj pdf) objects;
             let lengths = map String.length strings in
               let byte_offsets = 0 :: all_but_last (cumulative_sum 0 lengths) in
                 iter2
                   (fun o boff ->
                      output.Pdfio.output_string (string_of_int o);
                      output.Pdfio.output_string " ";
                      output.Pdfio.output_string (string_of_int boff);
                      output.Pdfio.output_string " ")
                   objects
                   byte_offsets;
                 let first = output.Pdfio.pos_out () in
                   iter output.Pdfio.output_string strings;
                   (extract_bytes_from_input_output output d, first)
       in
         let dict =
           Pdf.Dictionary
             [("/Type", Pdf.Name "/ObjStm");
              ("/Length", Pdf.Integer (bytes_size data));
              ("/N", Pdf.Integer (length objects));
              ("/First", Pdf.Integer first)]
         in
           let obj = Pdf.Stream {contents = (dict, Pdf.Got data)} in
             Pdfcodec.encode_pdfstream pdf Pdfcodec.Flate obj;
             Pdf.addobj_given_num pdf (tostream, obj))
    numbers

let print_data instream nonstream =
  if !write_debug then
    begin
      Printf.printf "Reserved for object streams: 1 to %i\n" (length instream);
      Printf.printf "Streams: ";
      print_ints (map fst instream);
      Printf.printf "\n";
      Printf.printf "Objects in streams: ";
      print_ints (flatten (map snd instream));
      flprint "\n";
      Printf.printf "Nonstream objects: ";
      print_ints nonstream;
      flprint "\n"
    end

(* Modify PDF to reinstate object streams from the saved hints. Then returns
the xref stream. Hints are now invalid, of course. Numbering scheme:
1...n   object streams
n+1...m objects in object streams
m+1...p objects not in object streams
p+1 xref stream *)
let reinstate_object_streams we_will_be_encrypting pdf =
  if !write_debug then flprint "pdf_to_output, reinstate object streams, so trying stream preservation\n";
  (* Adobe Reader can't cope with the document catalog being in a stream in an
  encrypted file, despite the ISO standard clearly allowing it. Make sure that,
  if we're encrypting, we remove any existing hint *)
  if we_will_be_encrypting then Hashtbl.remove pdf.Pdf.objects.Pdf.object_stream_ids pdf.Pdf.root;
  (* Build stream_objnum (streamnum, objects_for_this_stream) pairs. Take
  account of missing objects! *)
  let objects_for_streams =
    let table = null_hash ()
    and keys = null_hash () in
      Hashtbl.iter
        (fun objnum instream ->
           if not (Hashtbl.mem keys instream) then Hashtbl.add keys instream ();
           Hashtbl.add table instream objnum)
        pdf.Pdf.objects.Pdf.object_stream_ids;
      let lists = ref [] in
        Hashtbl.iter
          (fun instream _ -> lists =| (instream, Hashtbl.find_all table instream))
          keys;
      !lists
  in
  (* The nonstream objects are any not included in the objects_for_streams *)
  let nonstream_objects =
    let all_in_stream = null_hash () in
      iter (fun x -> Hashtbl.add all_in_stream x ()) (flatten (map snd objects_for_streams));
      option_map
        (fun x -> if not (Hashtbl.mem all_in_stream x) then Some x else None)
        (Pdf.objnumbers pdf)
  in
  print_data objects_for_streams nonstream_objects;
  (* Now renumber the PDF such that we have (1...n, [n + 1....m]) used for the
  and renumber the nonstream objects to (m + 1).... Also renumber
  objects_for_streams and nonstream_objects *)
  let n = length objects_for_streams in
  if !write_debug then Printf.printf "n = %i\n" n;
  let m = n + fold_left ( + ) 0 (map length (map snd objects_for_streams)) in
  if !write_debug then Printf.printf "m = %i\n" m;
  let changetable = null_hash () in
  (* Add all the objects_for_streams *)
  iter2 (Hashtbl.add changetable) (map fst objects_for_streams) (indx objects_for_streams);
  (* Add all the changes for the objects within streams *)
  let x = flatten (map snd objects_for_streams) in
    iter2 (Hashtbl.add changetable) x (indxn (n + 1) x);
  (* Add all the changes for the nonstream objects *)
  iter2 (Hashtbl.add changetable) nonstream_objects (indxn (m + 1) nonstream_objects);
  let pdf' = Pdf.renumber changetable pdf in
    pdf.Pdf.root <- pdf'.Pdf.root;
    pdf.Pdf.objects <- pdf'.Pdf.objects;
    pdf.Pdf.trailerdict <- pdf'.Pdf.trailerdict;
  (* Apply the changes to objects_for_streams and nonstream_objects *)
  let renumbered_nonstream_objects =
    map (Hashtbl.find changetable) nonstream_objects
  in
  let renumbered_objects_for_streams =
    combine
      (indx (map (Hashtbl.find changetable) (map fst objects_for_streams)))
      (map_lol (Hashtbl.find changetable) (map snd objects_for_streams))
  in
  print_data renumbered_objects_for_streams renumbered_nonstream_objects;
  (* Now build the object streams and bake them into the PDF *)
  bake_object_streams pdf renumbered_objects_for_streams;
  renumbered_objects_for_streams

(* Build the xref stream from collected data. The input xref positions 1..n and
m+1...p. We build xreferences to

a) The 1...n ones from xrefs
b) The n+1...m ones from renumbered_objects_for_streams
c) The m+1...p ones from xrefs

This stream is then returned - it will be written as object p + 1.
*)

(* Maximum bytes required to represent the numbers in a list *)
let max_bytes_required l =
  if l = [] then raise (Failure "max_bytes_required") else
    let r = ref (fold_left max min_int l)
    and b = ref 0 in
      while let v = !r > 0 in r := !r lsr 8; v do b += 1 done;
      max 1 !b

(* Output nbyyes bytes from x, highest byte first to the list reference given *)
let output_bytes nbytes x o =
  for pos = nbytes - 1 downto 0 do
    o.output_byte ((x land (255 lsl (pos * 8))) lsr (pos * 8))
  done

let make_xref_stream pdf xrefs renumbered_objects_for_streams =
  let entries = ref [(0, 65535, 0)] in
    (* 1...n ones from xrefs *)
    let type1s, type1s_tranche2 =
      cleave xrefs (length renumbered_objects_for_streams)
    in
      entries =@ rev (map (fun x -> (1, x, 0)) type1s);
      (* n+1...m ones from renumbered_objects_for_streams *)
      iter
        (fun (snum, objnums) ->
           entries =@ rev (map (fun i -> (2, snum, i)) (indx0 objnums)))
        renumbered_objects_for_streams;
      (* m+1...p ones from xrefs *)
      entries =@ rev (map (fun x -> (1, x, 0)) type1s_tranche2);
      let w1 = max_bytes_required (map (fun (x, _, _) -> x) !entries)
      and w2 = max_bytes_required (map (fun (_, x, _) -> x) !entries)
      and w3 = max_bytes_required (map (fun (_, _, x) -> x) !entries) in
        let data =
          let o, bytes = Pdfio.input_output_of_bytes 4096 in
            iter
              (function (typ, a, b) ->
                 output_bytes w1 typ o;
                 output_bytes w2 a o;
                 output_bytes w3 b o)
              (rev !entries);
            Pdfio.extract_bytes_from_input_output o bytes
        in
          let dict =
            Pdf.Dictionary
              (fold_right
                 (fun (k, v) d -> add k v d)
                 [("/Type", Pdf.Name "/XRef");
                  ("/Root", Pdf.Indirect pdf.Pdf.root);
                  ("/Size", Pdf.Integer (length !entries));
                  ("/W", Pdf.Array [Pdf.Integer w1; Pdf.Integer w2; Pdf.Integer w3]);
                  ("/Length", Pdf.Integer (bytes_size data))]
                 (match pdf.Pdf.trailerdict with Pdf.Dictionary d -> d | _ -> []))
          in
            let xrefstream = Pdf.Stream {contents = (dict, Pdf.Got data)} in
              Pdfcodec.encode_pdfstream
                pdf
                Pdfcodec.Flate
                ~predictor:Pdfcodec.PNGUp
                ~predictor_columns:(w1 + w2 + w3)
                xrefstream;
              xrefstream

(* Build hints for object streams from nothing, optionally preserving existing
streams. *)
let generate_object_stream_hints we_will_be_encrypting pdf preserve_existing =
  if !write_debug then Printf.printf "generate_object_stream_hints: %i existing hints, preserve_existing = %b"
    (Hashtbl.length pdf.Pdf.objects.Pdf.object_stream_ids) preserve_existing;
  if not preserve_existing then Hashtbl.clear pdf.Pdf.objects.Pdf.object_stream_ids;
  (* Adobe Reader can't cope with the document catalog being in a stream in an
  encrypted file, despite the ISO standard clearly allowing it. Make sure that,
  if we're encrypting, we remove any existing hint *)
  if !write_debug then Printf.printf "***** root (catalog) is object %i\n" pdf.Pdf.root;
  if we_will_be_encrypting then Hashtbl.remove pdf.Pdf.objects.Pdf.object_stream_ids pdf.Pdf.root;
  let biggest_hint =
    max
      (fold_left max min_int (map snd (list_of_hashtbl pdf.Pdf.objects.Pdf.object_stream_ids)))
      0
  in
    if !write_debug then Printf.printf "Biggest existing hint is %i\n" biggest_hint;
    let possibles =
      option_map
        (fun x ->
           if not (Hashtbl.mem pdf.Pdf.objects.Pdf.object_stream_ids x) then Some x else None)
        (Pdf.objnumbers pdf)
    in
      if !write_debug then
        Printf.printf "Found %i possible new objects to put into streams\n" (length possibles);
      let for_streams, indirect_lengths =
        let indirect_lengths = null_hash () in
          (option_map
            (fun x ->
               match Pdf.lookup_obj pdf x with
               | Pdf.Stream {contents = (Pdf.Dictionary d, _)} ->
                   begin
                     begin match lookup "/Length" d with
                     | Some (Pdf.Indirect i) -> Hashtbl.add indirect_lengths i ()
                     | _ -> ()
                     end;
                     None
                   end
               | _ -> Some x)
            possibles,
            indirect_lengths)
      in
        if !write_debug then Printf.printf
          "Got %i for_streams and %i indirect lengths\n"
          (length for_streams) (Hashtbl.length indirect_lengths);
        (* Remove indirect lengths and catalog. *)
        let final =
          option_map
            (fun x -> if Hashtbl.mem indirect_lengths x || (x = pdf.Pdf.root && we_will_be_encrypting) then None else Some x)
            for_streams
        in
          if !write_debug then
            Printf.printf "%i final objects for new object streams\n" (length final);
          let groups = splitinto 250 (sort compare final) in
            iter2
              (fun items groupnum ->
                 iter
                   (fun i ->
                      if !write_debug then
                        Printf.printf "Hinting object %i as being in group %i\n" i groupnum;
                      Hashtbl.add pdf.Pdf.objects.Pdf.object_stream_ids i groupnum)
                   items)
              groups
              (indxn (biggest_hint + 1) groups)

(* Flatten a PDF document to an Pdfio.output. *)
let pdf_to_output ?(preserve_objstm = false) ?(generate_objstm = false) linearize encrypt pdf o =
  if !write_debug then
    Printf.printf "pdf_to_output: preserve = %b, generate = %b, linearize = %b\n"
    preserve_objstm generate_objstm linearize;
  if linearize then pdf_to_output_linearized encrypt pdf o else
  let renumbered_objects_for_streams, preserve_objstm =
    if generate_objstm then generate_object_stream_hints (match encrypt with Some _ -> true | _ -> false) pdf preserve_objstm;
    if (preserve_objstm || generate_objstm) && Hashtbl.length pdf.Pdf.objects.Pdf.object_stream_ids > 0
      then (reinstate_object_streams (match encrypt with Some _ -> true | _ -> false) pdf, true)
      else ([], false) (* Either we weren't asked to preserve, or nothing to put in streams *)
  in
    let pdf =
      if preserve_objstm || generate_objstm then pdf else
        begin match encrypt with
        | Some e when e.encryption_method = AlreadyEncrypted -> pdf (* Already been renumbered *)
        | Some _  -> Pdf.renumber (Pdf.changes pdf) pdf (* Need to renumber before encrypting. Will remove once encryption-on-demand-on-writing is done...*)
        | _ -> pdf
        end
    in
      if !write_debug then flprint "Finished renumber\n";
      let pdf = crypt_if_necessary pdf encrypt in
        if !write_debug then
          begin
            flprint "crypt_if_necessary done...\n";
            if Pdfcrypt.is_encrypted pdf then flprint "FILE IS ENCRYPTED\n"
          end;
        o.output_string (header pdf);
        let xrefs = ref []
        and objiter =
          if Pdfcrypt.is_encrypted pdf || preserve_objstm || generate_objstm
            then Pdf.objiter_inorder
            else Pdf.objiter
        and changetable =
          if Pdfcrypt.is_encrypted pdf || preserve_objstm || generate_objstm
            then Hashtbl.create 0
            else Pdf.changes pdf
        and currobjnum = ref 1
        in
          if !write_debug then flprint "About to write objects\n";
          objiter
            (fun ob p ->
               xrefs =| o.pos_out ();
               strings_of_pdf_object
                 (flatten_W o) (ob, p) (if preserve_objstm then ob else !currobjnum) changetable;
               incr currobjnum)
            pdf;
          if !write_debug then flprint "finished writing objects\n";
          let xrefstart = o.pos_out () in
          if preserve_objstm || generate_objstm then
            begin
              let xrefstream =
                make_xref_stream pdf (rev !xrefs) renumbered_objects_for_streams
              in
                if !write_debug then
                  begin
                    flprint "Result of making xref stream\n";
                    flprint (string_of_pdf xrefstream);
                    flprint "OBJSTREAM trailer section...\n"
                  end;
                let thisnum =
                  match Pdf.lookup_direct pdf "/Size" xrefstream with
                  | Some (Pdf.Integer i) -> i
                  | _ -> failwith "bad xref stream generated\n"
                in
                  o.output_string (string_of_int thisnum ^ " 0 obj\n");
                  strings_of_pdf (flatten_W o) changetable xrefstream;
                  o.output_string "\nendobj\n";
                  o.output_string ("startxref\n" ^ string_of_int xrefstart ^ "\n%%EOF\n")
            end
          else
            begin
              if !write_debug then flprint "NORMAL NON-OBJSTREAM trailer section\n";
              write_xrefs (rev !xrefs) o;
              o.output_string "trailer\n";
              let trailerdict' =
                match pdf.Pdf.trailerdict with
                | Pdf.Dictionary trailerdict ->
                    Pdf.Dictionary
                      (add "/Size" (Pdf.Integer (length !xrefs + 1))
                        (add "/Root" (Pdf.Indirect pdf.Pdf.root) trailerdict))
                | _ ->
                    raise
                      (Pdf.PDFError "Pdf.pdf_to_channel: Bad trailer dictionary")
              in
                strings_of_pdf (flatten_W o) changetable trailerdict';
                if !write_debug then flprint "all done...\n";
                o.output_string
                  ("\nstartxref\n" ^ (string_of_int xrefstart) ^ "\n%%EOF\n")
            end

let change_id pdf f =
  match pdf.Pdf.trailerdict with
  | Pdf.Dictionary d ->
      {pdf with
         Pdf.trailerdict = Pdf.Dictionary (add "/ID" (Pdf.generate_id pdf f (fun () -> Random.float 1.)) d)}
  | _ -> raise (Pdf.PDFError "Bad trailer dictionary")

(* Write a PDF to a channel. Don't use mk_id when the file is encrypted.*)
let pdf_to_channel ?(preserve_objstm = false) ?(generate_objstm=false) linearize encrypt mk_id pdf ch =
  let pdf =
    if mk_id then change_id pdf "" else pdf
  in
    pdf_to_output ~preserve_objstm ~generate_objstm linearize encrypt pdf (output_of_channel ch)

(* Similarly to a named file. If mk_id is set, the /ID entry in the document's
trailer dictionary is updated using the current date and time and the filename.
Don't use mk_id when the file is encrypted. If [preserve_objstm] is set,
existing object streams will be preserved. If [generate_objstm] is set, new
ones will be generated in addition. To get totally fresh object streams, set
[preserve_objstm=false, generate_objstm=true]. *)
let pdf_to_file_options
  ?(preserve_objstm = false) ?(generate_objstm = false)
  linearize encrypt mk_id pdf f
=
  let pdf' =
    if mk_id then change_id pdf f else pdf
  in
    let ch = open_out_bin f in
      pdf_to_channel ~preserve_objstm ~generate_objstm linearize encrypt false pdf' ch;
      close_out ch

let pdf_to_file pdf f =
  pdf_to_file_options ~preserve_objstm:false ~generate_objstm:false false None true pdf f

let pdf_to_file_recrypting original decrypted_and_modified userpw filename =
  let dummy_encryption =
    Some {encryption_method = AlreadyEncrypted; owner_password = ""; user_password = ""; permissions = []}
  in
    let copied = Pdf.deep_copy decrypted_and_modified in
      let recrypted = Pdfcrypt.recrypt_pdf original copied userpw in
        pdf_to_file_options false dummy_encryption false recrypted filename

