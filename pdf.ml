(* This module declares a data type which represents an Adobe PDF document,
and defines various simple operations on it. *)
open Pdfutil
open Pdfio

(* Predicate on characters delimiting entities. *)
let is_delimiter = function
  | '(' | ')' | '<' | '>' | '[' | ']' | '{' | '}' | '%' | '/' -> true
  | _ -> false

(* Streams of binary data, byte-addressable, can either be in memory (Got) or
still in an input channel (ToGet). It may have been decrypted or encrypted, but
the actual calculations deferred: this is indicated by the crypt record
element. *)

type saved_encryption =
  {from_get_encryption_values :
     Pdfcryptprimitives.encryption * string * string * int32 * string *
     string option * string option;
   encrypt_metadata : bool;
   perms : string}

type deferred_encryption =
  {crypt_type : Pdfcryptprimitives.encryption;
   file_encryption_key : string option;
   obj : int;
   gen : int;
   key : int array;
   keylength : int;
   r : int}

(*let print_deferred_encryption e =
  begin match e.crypt_type with
  | Pdfcryptprimitives.ARC4 (a, b) -> Printf.printf "crypt_type = ARC4 (%i, %i)\n" a b
  | Pdfcryptprimitives.AESV2 -> Printf.printf "crypt_type = AESV2\n"
  | Pdfcryptprimitives.AESV3 x -> Printf.printf "crypt_type = AESV3 %b\n" x
  end;
  begin match e.file_encryption_key with
  | None -> Printf.printf "file_encryption_key: None\n"
  | Some _ -> Printf.printf "file_encryption_key: Some\n"
  end;
  Printf.printf "key = ";
  Array.iter (fun x -> Printf.printf "%C" (char_of_int x)) e.key;
  Printf.printf "\n";
  Printf.printf "obj = %i, gen = %i\n" e.obj e.gen;
  Printf.printf "keylength = %i\n" e.keylength;
  Printf.printf "r = %i\n" e.r*)

type toget_crypt =
  | NoChange
  | ToDecrypt of deferred_encryption

type toget =
  {input : input;
   position : int;
   length : int;
   crypt : toget_crypt}

let toget ?(crypt = NoChange) input position length =
  {input = input; position = position; length = length; crypt = crypt}

let length_of_toget t = t.length
let position_of_toget t = t.position
let input_of_toget t = t.input

type stream =
  | Got of bytes
  | ToGet of toget

(* Type for individual PDF objects. A Name includes the initial `/'. A
Stream consists of a reference to a pair of the stream dictionary (another
pdfobject) and a stream. Thus a pdfobject is technically mutable.  However,
at the user level, it is intended to be immutable: changes should be limited to
encoding and decoding of the stream.

Note that pdfobjects are not always amenable to polymorphic equality testing,
since the Pdio.input in the ToGet part of a stream contains functional
values. *)
type pdfobject =
  | Null
  | Boolean of bool
  | Integer of int
  | Real of float
  | String of string
  | Name of string 
  | Array of pdfobject list
  | Dictionary of (string * pdfobject) list
  | Stream of (pdfobject * stream) ref
  | Indirect of int

(* For debug. Filled in by Pdfwrite *)
let string_of_pdf : (pdfobject -> string) ref = ref (function _ -> "") 

(* An object is either lexed, or needs to be lexed from a position in the
input.
  Parsed -
    Not from an object stream, fully parsed, not necessarily decrypted yet
  ParsedAlreadyDecrypted -
    Was from an object stream, decrypted already when object stream read
  ToParse -
    Not parsed yet. Needs to be read from an object, which may still be
    encrypted
  ToParseFromObjectStream -
    (stream object number, index in stream) Not parsed yet.
    Will come from an object stream.
*)

type objectdata =
  | Parsed of pdfobject
  | ParsedAlreadyDecrypted of pdfobject
  | ToParse
  | ToParseFromObjectStream of
      (int, int list) Hashtbl.t * int * int *
      (int -> int list -> (int * (objectdata ref * int)) list)

type pdfobjmap_key = int

type pdfobjmap = (int, objectdata ref * int) Hashtbl.t

let pdfobjmap_empty () : pdfobjmap = Hashtbl.create 500

let pdfobjmap_find key map = Hashtbl.find map key

let pdfobjmap_add key value map = Hashtbl.replace map key value; map

let pdfobjmap_bindings_inorder map =
  let r = ref [] in Hashtbl.iter (fun k v -> r := (k, v)::!r) map;
  sort (fun (a, _) (b, _) -> compare a b) !r

let pdfobjmap_iter_inorder f map =
  iter (function (k, v) -> f k v) (pdfobjmap_bindings_inorder map)

let pdfobjmap_bindings map =
  let r = ref [] in Hashtbl.iter (fun k v -> r := (k, v)::!r) map; !r

let pdfobjmap_iter = Hashtbl.iter

let pdfobjmap_remove key map = Hashtbl.remove map key; map

(* We hold the maximum object number in use, maxobjnum to allow easy
production of new keys for the map. *)
type pdfobjects =
  {mutable maxobjnum : int;
   mutable parse : (pdfobjmap_key -> pdfobject) option;
   mutable pdfobjects : pdfobjmap;
   mutable object_stream_ids : (int, int) Hashtbl.t}

(* PDF Document. The major and minor version numbers, the root object number,
the list of objects and the trailer dictionary.

This represents the contents of a PDF file's user objects (object streams and
other mechanisms involved only in reading and writing are abstracted away). *)
type t =
  {mutable major : int; 
   mutable minor : int;
   mutable root : int;
   mutable objects : pdfobjects; 
   mutable trailerdict : pdfobject;
   mutable was_linearized : bool;
   mutable saved_encryption : saved_encryption option}

(* The null PDF document. *)
let empty () =
  {major = 2;
   minor = 0;
   root = 0;
   objects =
     {maxobjnum = 0;
      parse = None;
      pdfobjects = pdfobjmap_empty ();
      object_stream_ids = null_hash ()};
   trailerdict = Dictionary [];
   was_linearized = false;
   saved_encryption = None}

(* General exception for low-level errors. *)
exception PDFError of string

let input_pdferror i s =
  Printf.sprintf
    "%s whilst reading file %s at position %i"
    s i.Pdfio.source (i.Pdfio.pos_in ())

(* Predicate on those characters considered whitespace in PDF files. *)
let is_whitespace = function
  | '\000' | '\009' | '\010' | '\012' | ' ' | '\013' -> true
  | _ -> false

let is_not_whitespace = function
  | '\000' | '\009' | '\010' | '\012' | ' ' | '\013' -> false
  | _ -> true

let process_deferred_cryption toget_crypt data =
  match toget_crypt with
    NoChange -> data
  | ToDecrypt saved ->
      (*Printf.printf "Forcing...\n";
      print_deferred_encryption saved;*)
      Pdfcryptprimitives.decrypt_stream_data
        saved.crypt_type
        false
        saved.file_encryption_key
        saved.obj
        saved.gen
        saved.key
        saved.keylength
        saved.r
        data

let remove_string_compare (k' : string) l =
  let rec remove_inner r (k' : string) = function
    | [] -> r
    | (k, _)::t when k = k' -> List.rev_append r t
    | h::t -> remove_inner (h::r) k' t
  in
    remove_inner [] k' l

(* Remove a dictionary entry. Also works for streams. *)
let rec remove_dict_entry dict key =
  match dict with
  | Dictionary d -> Dictionary (remove_string_compare key d)
  | Stream ({contents = (dict', stream)} as s) ->
      s := (remove_dict_entry dict' key, stream);
      Stream s
  | _ -> raise (PDFError "remove_dict_entry: not a dictionary")

(* Replace dict entry, raising Not_found if it's not there. Also works
for streams. *)
let rec replace_dict_entry dict key value =
  match dict with
  | Null -> Dictionary (replace key value [])
  | Dictionary d -> Dictionary (replace key value d)
  | Stream ({contents = (dict', stream)} as s) ->
      s := (replace_dict_entry dict' key value, stream);
      Stream s
  | _ -> raise (PDFError "replace_dict_entry: not a dictionary.")

(* Add a dict entry, replacing if there. Also works for streams. *)
let rec add_dict_entry dict key value =
  match dict with
  | Null -> Dictionary (add key value [])
  | Dictionary d -> Dictionary (add key value d)
  | Stream ({contents = (dict', stream)} as s) ->
      s := (add_dict_entry dict' key value, stream);
      Stream s
  | _ -> raise (PDFError "add_dict_entry: not a dictionary.")

(* Get a stream from disk if it hasn't already been got. *)
let getstream = function
  | Stream ({contents = (d, ToGet {input = i; position = o; length = l; crypt = crypt})} as stream) ->
      if l = 0 then stream := (d, Got (mkbytes 0)) else
        begin try
          let data =
            process_deferred_cryption crypt (Pdfio.bytes_of_input i o l)
          in
            (* Correct the length *)
            let d' =
              match d with
                Dictionary _ -> replace_dict_entry d "/Length" (Integer (bytes_size data))
              | _ -> d (* May be null in Pdfwrite.WStream *)
            in
              stream := (d', Got data)
        with
          e ->
            raise
              (PDFError
                ("Pdf.getstream: can't read stream" ^ Printexc.to_string e))
        end
  | Stream {contents = (_, Got _)} -> ()
  | _ -> raise (PDFError "Pdf.getstream: not a stream")


let recurse_array (f : pdfobject -> pdfobject) elts =
  Array (map f elts)

(* Similarly for dictionaries. *)
let rec recurse_dict_inner preserve_order f prev = function
  | [] -> if preserve_order then rev prev else prev
  | (n, o)::t -> recurse_dict_inner preserve_order f ((n, f o)::prev) t

let recurse_dict ?(preserve_order=false) f elts =
  Dictionary (recurse_dict_inner preserve_order f [] elts)

let change_obj doc i obj =
  fst (pdfobjmap_find i doc.objects.pdfobjects) := Parsed obj

(* Parse an object [n] in document [pdf], updating the object in the document so
it is ready-parsed should it be required again. *)
let parse_lazy pdf n =
  match pdf.objects.parse with
  | None -> assert false
  | Some f ->
      let obj = f n in
        change_obj pdf n obj;
        obj

(* Remove an object. *)
let removeobj doc o =
  doc.objects <-
    {doc.objects with pdfobjects = pdfobjmap_remove o doc.objects.pdfobjects}

(* Look up an object. On an error return Null *)
let rec lookup_obj doc i =
  try
    match fst (pdfobjmap_find i doc.objects.pdfobjects) with
    | {contents = Parsed obj | ParsedAlreadyDecrypted obj} -> obj
    | {contents = ToParse} -> parse_lazy doc i
    | {contents =
        ToParseFromObjectStream (themap, streamobjnum, _, objstreamparser)} ->
         parse_delayed_object_stream themap i streamobjnum doc objstreamparser
  with
    Not_found -> Null

(* When we encounter a ToParseFromObjectStream, we:
a) Find all the ToParseFromObjectStream objects in the PDF with the same stream
object number
b) Read the object stream in the usual way
c) Replace each object in the PDF with the parsed one, marked as already
decrypted.
d) Delete the object stream, since it is no longer required.
e) Return the new object. *)
and parse_delayed_object_stream themap objnum streamobjnum pdf objstreamparser =
  let indexes = Hashtbl.find themap objnum in
    let objectsfromstream = objstreamparser streamobjnum indexes in
      iter
        (function (objnum, newobject) ->
           pdf.objects.pdfobjects <-
             pdfobjmap_add objnum newobject pdf.objects.pdfobjects)
        objectsfromstream;
      removeobj pdf streamobjnum;
      (* In the event that the object number we're looking for wasn't actually
      in the object stream due to a malformed file, we would enter an infinite
      loop parse_delayed_object_stream -> lookup_obj ->
      parse_delayed_object_stream etc. Check object was actually returned to
      avoid that. *)
      if mem objnum (map fst objectsfromstream)
        then lookup_obj pdf objnum
        else Null

(* Parse all object streams in a document *)
let resolve_all_delayed_object_streams pdf =
  iter
    (function (n, _) -> ignore (lookup_obj pdf n))
    (pdfobjmap_bindings pdf.objects.pdfobjects)

(* Return a float from a PDF number. *)
let rec getnum pdf = function
  | Real a -> a
  | Integer a -> float a
  | Indirect i -> getnum pdf (lookup_obj pdf i)
  | _ -> raise (PDFError "Pdf.getnum: not a number")

(* Parse a PDF rectangle data structure. Returns min x, min y, max x, max y. *)
let parse_rectangle pdf = function
  | Array [a; b; c; d] ->
      begin try
        let x, y, x', y' =
          getnum pdf a, getnum pdf b, getnum pdf c, getnum pdf d
        in
          fmin x x', fmin y y', fmax x x', fmax y y'
      with
        PDFError _ -> raise (PDFError "Pdf.parse_rectangle: bad rectangle")
      end
  | _ -> raise (PDFError "Pdf.parse_rectangle: not a rectangle")

let catalog_of_pdf pdf =
  try lookup_obj pdf pdf.root with
    Not_found -> raise (PDFError "No catalog")

(* Given any pdf document and object, follow indirections to yield a
direct object. A hanging indirect is defined as Null. *)
let rec direct pdf = function
  | Indirect i ->
      begin try
        match fst (pdfobjmap_find i pdf.objects.pdfobjects) with
        | {contents = Parsed pdfobject | ParsedAlreadyDecrypted pdfobject} ->
            direct pdf pdfobject
        | {contents = ToParse} -> parse_lazy pdf i
        | {contents =
             ToParseFromObjectStream (themap, streamobjnum, _, objstreamparser)}
           ->
             parse_delayed_object_stream
               themap i streamobjnum pdf objstreamparser
      with
        Not_found -> Null
      end
  | obj -> obj

(* Iterate over a stream. *)
let iter_stream f pdf =
  let rec iter_stream_inner f i = function
    | ({contents = ParsedAlreadyDecrypted (Stream _ as stream)}
    | {contents = Parsed (Stream _ as stream)}), _ -> f stream
    | {contents = ToParse}  as r, g ->
        r := Parsed (parse_lazy pdf i);
        iter_stream_inner f i (r, g)
    | {contents = ToParseFromObjectStream _}, _ ->
        (* Can't be any streams in here.. *)
        ()
    | _ -> ()
  in
    pdfobjmap_iter (iter_stream_inner f) pdf.objects.pdfobjects

(* Lookup a key in a dictionary, following indirect references,  returning
None on any failure. This works on both plain dictionaries and streams. *)
let rec lookup_string_compare (k' : string) = function
  | [] -> None
  | (k, v)::t -> if k = k' then Some v else lookup_string_compare k' t

let lookup_direct pdf key dict =
  match direct pdf dict with
  | Dictionary d | Stream {contents = (Dictionary d, _)} ->
      begin match lookup_string_compare key d with
      | None | Some Null -> None
      | Some o -> Some (direct pdf o)
      end
  | _ -> None

let lookup_immediate key dict =
  match dict with
  | Dictionary d | Stream {contents = (Dictionary d, _)} -> lookup_string_compare key d
  | _ -> None

let lookup_direct_or_array pdf name obj =
  match explode name with
  | '/'::'['::num ->
      let digits, _ = cleavewhile isdigit num in
        begin match obj with
        | Array a -> Some (direct pdf (List.nth a (int_of_string (implode digits))))
        | _ -> None
        end
  | _ -> lookup_direct pdf name obj

(* Follow a nested chain of dictionary entries. *)
let rec lookup_chain pdf obj = function
  | [] -> Some obj
  | [n] -> lookup_direct_or_array pdf n obj
  | n::ns ->
      match lookup_direct_or_array pdf n obj with
      | Some obj' -> lookup_chain pdf obj' ns
      | None -> None

let indirect_number pdf key dict =
  match direct pdf dict with
  | Dictionary d | Stream {contents = (Dictionary d, _)} ->
      begin match lookup_string_compare key d with
      | Some (Indirect i) ->
          (* If the indirect points to an indirect, this will still be unique,
          but isn't the "actual" object number *)
          Some i
      | _ -> None
      end
  | _ -> None

(* Add an object, given an object number. *)
let addobj_given_num doc (num, obj) =
  doc.objects.maxobjnum <-
    max doc.objects.maxobjnum num;
  doc.objects.pdfobjects <-
    pdfobjmap_add num (ref (Parsed obj), 0) doc.objects.pdfobjects

(* Follow a chain from the root, finding a dictionary entry to replace (or add).
   Keep the same direct / indirect structure as is already present - any
   terminal chain of previously-absent dictionaries will be created in direct
   form. *)

(* Find the final indirect object in the chain, returning its number and the
   remaining (fully-direct) chain *)
let rec find_final_indirect remaining_chain pdf obj objnum = function
  | [] -> (objnum, rev remaining_chain)
  | k::ks ->
      match explode k with
      | '/'::'['::num ->
          let digits, _ = cleavewhile isdigit num in
            begin match obj with
            | Array a ->
                begin match List.nth a (int_of_string (implode digits)) with
                | Indirect i -> find_final_indirect [] pdf (lookup_obj pdf i) i ks
                | newobj -> find_final_indirect (k::remaining_chain) pdf newobj objnum ks
                end
            | _ -> assert false (* chain pre-checked by lookup_chain *)
            end
      | _ ->
         match indirect_number pdf k obj with
         | Some i -> find_final_indirect [] pdf (lookup_obj pdf i) i ks
         | None ->
             match lookup_immediate k obj with
             | Some obj -> find_final_indirect (k::remaining_chain) pdf obj objnum ks
             | None -> assert false (* chain pre-checked by lookup_chain *)

(* The object number is now chosen. We follow what remains of the chain and insert
the new key-value pair. *)
let rec replace_chain_all_direct finalobj chain (k, v) =
  match finalobj with
  | Dictionary dd as d | Stream ({contents = (Dictionary dd as d, _)}) ->
      begin match chain with
      | [] -> add_dict_entry d k v
      | c::cs -> add_dict_entry d c (replace_chain_all_direct (unopt (lookup c dd)) cs (k, v))
      end
  | Array a ->
      begin match chain with
      | [] ->
          (* Put the value in the array at the position indictated by the 'key' *)
          begin match explode k with
          | '/'::'['::num ->
              let digits, _ = cleavewhile isdigit num in
              let n = int_of_string (implode digits) in
                Array (List.mapi (fun n' e -> if n' = n then v else e) a)
          | _ -> 
          raise (PDFError "replace_chain_all_direct: nothing to put in array")
          end
      | c::cs ->
          match explode c with
          | '/'::'['::num ->
              let digits, _ = cleavewhile isdigit num in
              let n = int_of_string (implode digits) in
                Array (List.mapi (fun n' e -> if n' = n then (replace_chain_all_direct e cs (k, v)) else e) a)
          | _ -> raise (PDFError "replace_chain_all_direct: bad array chain")
      end
  | _ -> raise (PDFError "replace_chain_all_direct: bad chain")

let replace_chain_exists pdf chain (k, v) =
  match lookup_chain pdf pdf.trailerdict chain with
  | None -> raise (PDFError "chain must already exist")
  | Some _ ->
      match chain with
      | [] -> raise (PDFError "no chain")
      | chain ->
          let finalobjnum, remaining_chain = find_final_indirect [] pdf pdf.trailerdict 0 chain in
            let newobj =
              replace_chain_all_direct (if finalobjnum = 0 then pdf.trailerdict else lookup_obj pdf finalobjnum) remaining_chain (k, v)
            in
              if finalobjnum = 0 then pdf.trailerdict <- newobj else addobj_given_num pdf (finalobjnum, newobj)

let replace_chain pdf chain obj =
  let rec find_max_existing to_fake chain =
    if chain = [] then (chain, to_fake) else
      match lookup_chain pdf pdf.trailerdict chain with
      | None -> find_max_existing (hd (rev chain)::to_fake) (rev (tl (rev chain)))
      | _ -> (chain, to_fake)
  in
  let rec wrap_obj obj = function
  | [] -> obj
  | h::t -> Dictionary [(h, wrap_obj obj t)]
  in
    let chain, to_fake = find_max_existing [] chain in
      let chain, key, obj =
        match to_fake with
        | [] -> (rev (tl (rev chain)), hd (rev chain), obj)
        | h::t -> (chain, h, wrap_obj obj t)
      in
        if chain = [] then
          pdf.trailerdict <- add_dict_entry pdf.trailerdict key obj
        else
          replace_chain_exists pdf chain (key, obj)

(* FIXME When the final item in the chain is indirect, shouldn't it be kept indirect and the object just replaced? In case there are other references to it? Check. *)

(* Remove a dictionary entry given its chain. Returns true if removed.
   Our example for now /Root/Names/JavaScript. Aim is to remove the dict entry with key "/JavaScript" from /Names.
   In the future, extend to deleting things other than dictionary entries. *)
let rec remove_chain_all_direct chain obj =
  match chain with
  | [] -> obj
  | [k] ->
      (*Printf.printf "remove_chain_all_direct, final k = %s, obj = %s\n" k (!string_of_pdf obj);*)
      begin match obj with
      | Dictionary _ as d ->
          remove_dict_entry d k
      | Stream ({contents = (Dictionary _ as d, s)} as r) ->
          r := (remove_dict_entry d k, s); obj
      | _ -> assert false
      end
  | k::ks ->
      (*Printf.printf "remove_chain_all_direct, k = %s, obj = %s\n" k (!string_of_pdf obj);*)
      begin match obj with
      | Dictionary dd as d ->
          add_dict_entry d k (remove_chain_all_direct ks (unopt (lookup k dd)))
      | Stream ({contents = (Dictionary dd as d, s)} as r) ->
          r := (add_dict_entry d k (remove_chain_all_direct ks (unopt (lookup k dd))), s);
          obj
      | _ -> assert false
      end

let remove_chain pdf chain =
  (*flprint  "remove_chain\n";*)
  match lookup_chain pdf pdf.trailerdict chain with
  | None -> false
  | Some _ ->
      match chain with
      | [] | [_] -> raise (PDFError "no chain or chain too short")
      | chain ->
          let finalobjnum, remaining_chain = find_final_indirect [] pdf pdf.trailerdict 0 (all_but_last chain) in
          let remaining_chain = remaining_chain @ [last chain] in
          (*Printf.printf "finalobjnum = %i = %s\n" finalobjnum (!string_of_pdf (lookup_obj pdf finalobjnum));*)
          let newobj =
            if finalobjnum = 0 then
              remove_chain_all_direct remaining_chain pdf.trailerdict
            else
              remove_chain_all_direct remaining_chain (lookup_obj pdf finalobjnum)
          in
            (*Printf.printf "newobj = %s\n" (!string_of_pdf newobj);*)
            (if finalobjnum = 0 then pdf.trailerdict <- newobj else addobj_given_num pdf (finalobjnum, newobj));
            true

(* Look up under a key and its alternate. Return the value associated
with the key that worked, or [None] if neither did. *)
let lookup_direct_orelse pdf k k' d =
  match lookup_direct pdf k d with
  | None -> lookup_direct pdf k' d
  | result -> result

(* Look something up in a dictionary, failing with given exception if not
found. We make direct both the dictionary and the result of the lookup. This
also allows us to look things up in a stream dictionary transparently. *)
let lookup_exception (exp : exn) pdf key dict =
  let dict' =
    match direct pdf dict with
    | Dictionary d | Stream {contents = Dictionary d, _} -> d
    | _ -> raise exp
  in
    match lookup key dict' with
    | None | Some Null -> raise exp
    | Some v -> direct pdf v

(* A specialised one raising PDFError. *)
let lookup_fail text =
  lookup_exception (PDFError text)

(* Parse a matrix. *)
let parse_matrix pdf name dict =
  match lookup_direct pdf name dict with
  | None -> Pdftransform.i_matrix
  | Some (Array [a; b; c; d; e; f]) ->
      let a = getnum pdf a in let b = getnum pdf b in let c = getnum pdf c
      in let d = getnum pdf d in let e = getnum pdf e in let f = getnum pdf f in
        {Pdftransform.a = a; Pdftransform.b = b; Pdftransform.c = c;
         Pdftransform.d = d; Pdftransform.e = e; Pdftransform.f = f}
  | _ -> raise (PDFError "Malformed matrix")

(* Make a matrix *)
let make_matrix tr =
  Array
    [Real tr.Pdftransform.a; Real tr.Pdftransform.b; Real tr.Pdftransform.c;
     Real tr.Pdftransform.d; Real tr.Pdftransform.e; Real tr.Pdftransform.f]

(* Iterate over the objects in a document, in order of increasing
object number. *)
let objiter f doc =
  (* PdfObjMap.iter doesn't like you altering the map inside the iteration. *)
  resolve_all_delayed_object_streams doc;
  let f' k v =
    match v with
    | {contents = Parsed obj}, _ -> f k obj
    | {contents = ParsedAlreadyDecrypted obj}, _ -> f k obj
    | {contents = ToParse}, _ -> f k (parse_lazy doc k)
    | {contents = ToParseFromObjectStream (themap, s, _, func)}, _ ->
         f k (parse_delayed_object_stream themap k s doc func)
  in
    pdfobjmap_iter f' doc.objects.pdfobjects

let objselect p doc =
  let ns = ref [] in
    objiter (fun n obj -> if p obj then ns =| n) doc;
    !ns

let objselfmap f doc =
  resolve_all_delayed_object_streams doc;
  let rec f' k v =
    match v with
    | {contents = Parsed obj} as r, _ ->
        r := Parsed (f obj)
    | {contents = ParsedAlreadyDecrypted obj} as r, _ ->
        r := ParsedAlreadyDecrypted (f obj)
    | {contents = ToParse}, _ ->
        ignore (parse_lazy doc k); f' k v
    | {contents = ToParseFromObjectStream (themap, s, _, func)}, _ ->
        ignore (parse_delayed_object_stream themap k s doc func);
        f' k v
  in
    pdfobjmap_iter f' doc.objects.pdfobjects

let objiter_inorder f doc =
  (* PdfObjMap.iter doesn't like you altering the map inside the iteration. *)
  resolve_all_delayed_object_streams doc;
  let f' k v =
    match v with
    | {contents = Parsed obj}, _ -> f k obj
    | {contents = ParsedAlreadyDecrypted obj}, _ -> f k obj
    | {contents = ToParse}, _ -> f k (parse_lazy doc k)
    | {contents = ToParseFromObjectStream (themap, s, _, func)}, _ ->
         f k (parse_delayed_object_stream themap k s doc func)
  in
    pdfobjmap_iter_inorder f' doc.objects.pdfobjects

(* Same, but also pass generation number. *)
let objiter_gen f doc =
  (* PdfObjMap.iter doesn't like you altering the map inside the iteration. *)
  resolve_all_delayed_object_streams doc;
  let f' k v =
    match v with
    | {contents = Parsed obj}, g -> f k g obj
    | {contents = ParsedAlreadyDecrypted obj}, g -> f k g obj
    | {contents = ToParse}, g -> f k g (parse_lazy doc k)
    | {contents = ToParseFromObjectStream (themap, s, _, func)}, g ->
         f k g (parse_delayed_object_stream themap k s doc func)
  in
    pdfobjmap_iter f' doc.objects.pdfobjects

(* Return a list of object numbers. *)
let objnumbers pdf =
  let keys = ref [] in
    objiter (fun k _ -> keys =| k) pdf;
    rev !keys

(* Cardinality of object set. O(n). *)
let objcard pdf =
  let card = ref 0 in
    objiter (fun _ _ -> incr card) pdf;
    !card

(* Return a list of (k, v) pairs. *)
let list_of_objs doc =
  let objs = ref [] in
    objiter (fun k v -> objs =| (k, Parsed v)) doc;
    !objs


(* Add an object. We use the first number larger than the maxobjnum,
and update that. *)
let addobj doc obj =
  let num = doc.objects.maxobjnum + 1 in
    addobj_given_num doc (num, obj);
    num

(* Make a objects entry from a list of (number, object) pairs. *)
let objects_of_list parse l =
  let maxobj = ref 0
  in let map = ref (pdfobjmap_empty ()) in
    iter
      (fun (k, v) ->
         maxobj := max !maxobj k;
         map := pdfobjmap_add k v !map)
      l;
    {parse = parse;
     pdfobjects = !map;
     maxobjnum = !maxobj;
     object_stream_ids = null_hash ()}

(* Find the page reference numbers, given the top level node of the page tree *)
let rec page_reference_numbers_inner pdf pages_node node_number =
  match lookup_direct pdf "/Type" pages_node with
    Some (Name "/Page") -> [node_number]
  | _ ->
      match lookup_direct pdf "/Kids" pages_node with
        Some (Array elts) ->
          flatten
            (option_map
              (function
               | Indirect i ->
                   Some
                     (page_reference_numbers_inner
                        pdf (direct pdf (Indirect i)) i)
               | _ -> None)
              elts)
      | _ ->
          (* Missing /Type /Page in a malformed file would end up here *)
          [node_number]

let page_reference_numbers pdf =
  let root = lookup_obj pdf pdf.root in
    let pages_node =
      match lookup_direct pdf "/Pages" root with
      | Some p -> p
      | None -> raise (PDFError "No /Pages found in /Root")
    in
      page_reference_numbers_inner pdf pages_node (-1)

(* Renumber an object given a change table (A hash table mapping old to new
numbers). *)
let rec renumber_object_parsed ~preserve_order pdf changes = function
  | Indirect i ->
      let i' =
        match tryfind changes i with
        | Some x -> x
        | None -> i (*r A dangling indirect is valid. *)
      in
        Indirect i'
  | Array a ->
      recurse_array (renumber_object_parsed ~preserve_order pdf changes) a
  | Dictionary d ->
      recurse_dict ~preserve_order (renumber_object_parsed ~preserve_order pdf changes) d
  | Stream {contents = (p, s)} ->
      Stream {contents = renumber_object_parsed ~preserve_order pdf changes p, s}
  | pdfobject -> pdfobject

let rec check_object_contains_renumbering changes = function
  | Indirect i ->
      begin match tryfind changes i with | Some x -> true | None -> false end
  | Array a ->
      List.exists (check_object_contains_renumbering changes) a
  | Dictionary d ->
      List.exists (fun (_, x) -> check_object_contains_renumbering changes x) d
  | Stream {contents = (Dictionary p, _)} ->
      List.exists (fun (_, x) -> check_object_contains_renumbering changes x) p
  | pdfobject -> false

let renumber_object ?(preserve_order=false) pdf changes objnum = function
  | ToParse -> 
      let o = parse_lazy pdf objnum in
        if check_object_contains_renumbering changes o then renumber_object_parsed ~preserve_order pdf changes o else o
  | ToParseFromObjectStream (themap, s, _, func) ->
      let o = parse_delayed_object_stream themap objnum s pdf func in
        if check_object_contains_renumbering changes o then renumber_object_parsed ~preserve_order pdf changes o else o
  | Parsed obj | ParsedAlreadyDecrypted obj ->
      if check_object_contains_renumbering changes obj then renumber_object_parsed ~preserve_order pdf changes obj else obj

(* Renumber a PDF's objects to 1...n. *)

(* Calculate the substitutions required to renumber the document. *)
let changes pdf =
  let card = objcard pdf in
    let order = ilist_fail_null 1 card
    and change_table = Hashtbl.create card in
      List.iter2 (Hashtbl.add change_table) (objnumbers pdf) order;
      change_table
      
(* Perform all renumberings given by a change table. *)
let renumber ?(preserve_order=false) change_table pdf =
  let root' =
    match tryfind change_table pdf.root with Some x -> x | None -> pdf.root
  and trailerdict' =
    renumber_object ~preserve_order pdf change_table 0 (Parsed pdf.trailerdict)
  and objects' =
    let nums, objs = split (list_of_objs pdf) in
      let objs' =
        map2 (renumber_object ~preserve_order pdf change_table) nums objs
      in let nums' =
        map
          (function k ->
            match tryfind change_table k with Some x -> x | None -> k)
          nums
      in
        objects_of_list
          pdf.objects.parse
          (combine nums' (map (fun x -> ref (Parsed x), 0) objs'))
  in
    (* Update the object_stream_ids so object streams will be
    conserved over PDF merges *)
    let newids = null_hash () in
      Hashtbl.iter
        (fun o s ->
           match tryfind change_table o with
             Some o' -> Hashtbl.add newids o' s
           | _ -> ())
        pdf.objects.object_stream_ids;
      objects'.object_stream_ids <- newids;
      {pdf with
       root = root';
       objects = objects';
       trailerdict = trailerdict'}
 
(* Renumber the objects (including root and trailer dictionary) in a list of
pdfs so they are mutually exclusive. We iterate over the key lists to build
a list of change tables which are applied to the input PDFs. NOTE: This can't
be used on PDFs where the generation numbers still matter (i.e before
decryption). *)
let renumber_pdfs pdfs =
  let keylists = map objnumbers pdfs
  and bse = ref 1
  and tables = ref [] in
    iter
      (fun k ->
         let length = length k in
           let table = Hashtbl.create length in
             List.iter2 (Hashtbl.add table) k (ilist !bse (!bse + length - 1));
             tables =| table;
             bse += length)
      keylists;
    map2 renumber (rev !tables) pdfs

(* Give a list of object numbers referenced in a given [pdfobject] *)
let rec allfalse = function
  | [] -> true
  | h::t -> not h && allfalse t 

let rec containing l a =
  match a, l with
  | _, [] -> false
  | (name, Name n), ((h, Name hh)::_) when name = h && n = hh -> true
  | _, (_::t) -> containing t a

let tocontinue no_follow_entries no_follow_contains d =
  (isnull no_follow_entries && isnull no_follow_contains) ||
  allfalse (map (containing d) no_follow_contains)

(* Sets of references *)
let refset_empty () = Hashtbl.create 500

let refset_add n rs = Hashtbl.replace rs n (); rs

let refset_mem n rs = Hashtbl.mem rs n

let refset_elts rs =
  let r = ref [] in Hashtbl.iter (fun k _ -> r := k::!r) rs; !r

let rec
  referenced_pdfobj no_follow_entries no_follow_contains pdf found i
= function
  | Indirect j ->
      if not (refset_mem j !found) then
        begin
          let obj = 
            try lookup_obj pdf j with
              Not_found -> Null
          in
            match obj with
            | Dictionary d ->
                if tocontinue no_follow_entries no_follow_contains d then
                  begin
                  found := refset_add j !found;
                  referenced_pdfobj
                    no_follow_entries no_follow_contains pdf found j obj
                  end
            | _ ->
              found := refset_add j !found;
              referenced_pdfobj
                no_follow_entries no_follow_contains pdf found j obj
        end
  | Dictionary d ->
      iter
        (function (_, v) ->
          referenced_pdfobj no_follow_entries no_follow_contains pdf found i v)
        (if no_follow_entries <> []
           then (lose (fun (k, _) -> mem k no_follow_entries) d)
           else d)
  | Array a -> 
      iter
        (referenced_pdfobj no_follow_entries no_follow_contains pdf found i)
        a
  | Stream {contents = (s, _)} ->
      referenced_pdfobj no_follow_entries no_follow_contains pdf found i s
  | _ -> ()

and referenced no_follow_entries no_follow_contains pdf found i = function
  | Parsed ((Indirect _ | Array _ | Dictionary _ | Stream _) as o)  ->
      referenced_pdfobj no_follow_entries no_follow_contains pdf found i o
  | ParsedAlreadyDecrypted x ->
      referenced no_follow_entries no_follow_contains pdf found i (Parsed x)
  | ToParse ->
      referenced
        no_follow_entries no_follow_contains pdf found i
        (Parsed (parse_lazy pdf i))
  | ToParseFromObjectStream (themap, s, _, func) ->
      let result = parse_delayed_object_stream themap i s pdf func in
        referenced
          no_follow_entries no_follow_contains pdf found i
          (ParsedAlreadyDecrypted result)
  | _ -> ()

(* Nullify all references to page objects which are no longer in the page tree.
This prevents (for instance) annotations on a page referencing a deleted page,
thus preventing the deleted page's objects from being retained during garbage
collection. *)
let nullify_deleted_page_references pdf =
  let rec nullify numbers = function
    | Indirect i when tryfind numbers i <> None -> Null
    | Array elts -> recurse_array (nullify numbers) elts
    | Dictionary elts -> recurse_dict (let p2 = nullify numbers in p2) elts
    | Stream {contents = (p, s)} -> Stream {contents = nullify numbers p, s}
    | x -> x
  and page_object_numbers =
    let nums = ref [] in
      objiter
        (let f2 = function objnum ->
           (let f1 = function
             | Dictionary d when
                   (match lookup "/Type" d with
                      Some (Name "/Page") -> true | _ -> false) ->
                 nums := objnum :: !nums
             | _ -> () in f1) in f2)
        pdf;
      !nums
  in
    let refnums = page_reference_numbers pdf in
      if length refnums <> length page_object_numbers then
        begin
          let table = Hashtbl.create 50 in
            iter (function x -> Hashtbl.add table x ()) page_object_numbers;
            iter (Hashtbl.remove table) refnums;
            objselfmap (nullify table) pdf
        end

(* Remove any unreferenced objects. *)
let remove_unreferenced pdf =
  nullify_deleted_page_references pdf;
  let found = ref (refset_empty ()) in
    referenced [] [] pdf found pdf.root (Parsed (lookup_obj pdf pdf.root));
    referenced [] [] pdf found 0 (Parsed pdf.trailerdict);
    found := refset_add pdf.root !found;
    let eltnumbers = refset_elts !found in
      (* If not found, just ignore. *)
      let elements =
        map (fun n -> try lookup_obj pdf n with Not_found -> Null) eltnumbers
      in
        pdf.objects <-
          {pdf.objects with
             maxobjnum = 0;
             pdfobjects = pdfobjmap_empty ()};
        iter (addobj_given_num pdf) (combine eltnumbers elements)

(* Objects referenced from a given one. *)
let objects_referenced no_follow_entries no_follow_contains pdf pdfobject =
  let set = ref (refset_empty ()) in
    referenced
      no_follow_entries no_follow_contains pdf set 0 (Parsed pdfobject);
    refset_elts !set


(* Find the contents of a stream as a bytes. *)
let bigarray_of_stream s =
  getstream s;
  match s with
  | Stream {contents = _, Got bytes} -> bytes
  | _ -> raise (PDFError "couldn't extract raw stream")

(* Given a dictionary and a prefix (e.g gs), return a name, starting with the
prefix, which is not already in the dictionary (e.g /gs0). *)
let unique_key prefix obj =
  let elts = match obj with
    | Dictionary es
    | Stream {contents = Dictionary es, _} -> es
    | _ -> raise (PDFError "unique_key: Not a dictionary or stream")
  in
    let names = fst (split elts) in
    let name_of_num n = Printf.sprintf "/%s%i" prefix n in
    let num = ref 0 in
      while mem (name_of_num !num) names do incr num done;
      name_of_num !num

(* Given a PDF and potential filename, calculate an MD5 string and build a
suitable /ID entry from it. *)
let generate_id _ path gettime =
  let d =
    match Sys.getenv_opt "CAMLPDF_REPRODUCIBLE_IDS" with
    | Some "true" -> Digest.string "camlpdf"
    | _ -> Digest.string (path ^ string_of_float (gettime ()))
  in
    Array [String d; String d]

(* Find the indirect reference given by the value associated with a key in a
dictionary. *)
let find_indirect key dict =
  match dict with
  | Dictionary d ->
      begin match lookup key d with
      | Some (Indirect i) -> Some i
      | _ -> None
      end
  | _ -> raise (PDFError "find_indirect: not a dictionary")

(* Look something up in a name tree. *)
let rec nametree_lookup_kids pdf k = function
  | Array (h::t) ->
      begin match nametree_lookup pdf k h with
      | None ->
          nametree_lookup_kids pdf k (Array t)
      | Some result -> Some result
      end
  | Array [] -> None
  | _ -> raise (PDFError "nametree_lookup_kids: malformed name tree")

and array_lookup k = function
  | Array elts ->
      lookup k (pairs_of_list elts)
  | _ -> raise (PDFError "Bad lookup array")

and nametree_lookup pdf k dict =
  match lookup_direct pdf "/Limits" dict with
  | Some (Array [l;r]) ->
      (* Check we're not searching in a malformed tree *)
      if k < l || k > r then None else
      begin match lookup_direct pdf "/Kids" dict with
      | Some kids ->
          (* Intermediate node *)
          nametree_lookup_kids pdf k kids
      | None ->
          match lookup_direct_orelse pdf "/Names" "/Nums" dict with
          | Some names ->
              (* leaf node *)
              array_lookup k names
          | None ->
              raise (PDFError "Malformed name tree entry")
      end
  | None ->
      begin match lookup_direct pdf "/Kids" dict with
      | Some kids ->
          (* Root node with kids *)
          nametree_lookup_kids pdf k kids
      | None ->
          match lookup_direct_orelse pdf "/Names" "/Nums" dict with
          | Some names ->
              (* Root node with names *)
              array_lookup k names
          | None ->
              raise (PDFError "Missing name tree entry")
      end
  | _ -> raise (PDFError "Malformed name tree")

(* Return an ordered list of all the (k, v) pairs in a tree *)
let rec contents_of_nametree pdf tree =
  match lookup_direct_orelse pdf "/Names" "/Nums" tree with
  | Some (Array names) ->
      let rec pairs_of_list prev = function
      | [] -> rev prev
      | [_] ->
          Pdfe.log "warning: contents_of_nametree: odd number of /Names\n";
          rev prev
      | k::v::r -> pairs_of_list ((k, v)::prev) r
      in
        pairs_of_list [] names
  | _ ->
      match lookup_direct pdf "/Kids" tree with
      | Some (Array kids) ->
          flatten (map (contents_of_nametree pdf) kids)
      | _ -> raise (PDFError "contents_of_nametree: neither names nor kids")

let deep_copy_pdfobjects frompdf from =
  resolve_all_delayed_object_streams frompdf;
  let deep_copy_objdata objdata =
    let deep_copy_pdfobject = function
      | Stream _ as s ->
          begin
            getstream s;
            match s with
            | Stream {contents = (dict, Got stream)} ->
                Stream (ref (dict, Got (copybytes stream)))
            | _ ->
                (* getstream only returns things of above form *)
                assert false
          end
      | x -> x
    in
      match objdata with
      | Parsed obj -> Parsed (deep_copy_pdfobject obj)
      | ParsedAlreadyDecrypted obj ->
          ParsedAlreadyDecrypted (deep_copy_pdfobject obj)
      | ToParse -> ToParse
      | ToParseFromObjectStream (themap, x, y, z) ->
          ToParseFromObjectStream (themap, x, y, z)
  in
    (* shouldn't occur due to resolve_all_delayed_object_streams above -
    do we really need that? *)
    let pdfobjmap = pdfobjmap_empty () in
      pdfobjmap_iter
        (fun objnum ({contents = objdata}, gen) ->
           ignore
             (pdfobjmap_add
                objnum (ref (deep_copy_objdata objdata), gen) pdfobjmap))
        from;
      pdfobjmap

let deep_copy from =
  {major = from.major;
   minor = from.minor;
   root = from.root;
   objects =
     {maxobjnum = from.objects.maxobjnum;
      parse = from.objects.parse;
      pdfobjects = deep_copy_pdfobjects from from.objects.pdfobjects;
      object_stream_ids = Hashtbl.copy from.objects.object_stream_ids};
   trailerdict = from.trailerdict;
   was_linearized = from.was_linearized;
   saved_encryption = from.saved_encryption}

let change_id pdf f =
  match pdf.trailerdict with
  | Dictionary d ->
      pdf.trailerdict <-
        Dictionary (add "/ID" (generate_id pdf f (fun () -> Random.float 1.)) d)
  | _ -> raise (PDFError "Bad trailer dictionary")

let transform_rect pdf transform rect =
  let minx, miny, maxx, maxy = parse_rectangle pdf rect in
    let (x0, y0) = Pdftransform.transform_matrix transform (minx, miny) in
    let (x1, y1) = Pdftransform.transform_matrix transform (maxx, maxy) in
    let (x2, y2) = Pdftransform.transform_matrix transform (minx, maxy) in
    let (x3, y3) = Pdftransform.transform_matrix transform (maxx, miny) in
      let minx = fmin (fmin x0 x1) (fmin x2 x3) in
      let miny = fmin (fmin y0 y1) (fmin y2 y3) in
      let maxx = fmax (fmax x0 x1) (fmax x2 x3) in
      let maxy = fmax (fmax y0 y1) (fmax y2 y3) in
        Array [Real minx; Real miny; Real maxx; Real maxy]

let transform_quadpoint_single pdf transform = function
  | [x1; y1; x2; y2; x3; y3; x4; y4] ->
      let x1, y1, x2, y2, x3, y3, x4, y4 =
        getnum pdf x1, getnum pdf y1,
        getnum pdf x2, getnum pdf y2,
        getnum pdf x3, getnum pdf y3,
        getnum pdf x4, getnum pdf y4
      in
        let (x1, y1) = Pdftransform.transform_matrix transform (x1, y1) in
        let (x2, y2) = Pdftransform.transform_matrix transform (x2, y2) in
        let (x3, y3) = Pdftransform.transform_matrix transform (x3, y3) in
        let (x4, y4) = Pdftransform.transform_matrix transform (x4, y4) in
          map (fun x -> Real x) [x1; y1; x2; y2; x3; y3; x4; y4]
  | qp ->
     Pdfe.log "Malformed /QuadPoints format: must be a multiple of 8 entries\n";
     qp

let transform_quadpoints pdf transform = function
| Array qps ->
    Array (flatten (map (transform_quadpoint_single pdf transform) (splitinto 8 qps)))
| qp ->
    Pdfe.log (Printf.sprintf "Unknown or malformed /QuadPoints format %s\n" (!string_of_pdf qp));
    qp
