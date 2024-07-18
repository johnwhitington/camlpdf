open Pdfutil

(* Reading a name tree *)
let rec read_name_tree pdf tree =
  let names =
    match Pdf.lookup_direct_orelse pdf "/Names" "/Nums" tree with
    | Some (Pdf.Array elts) ->
        if odd (length elts)
          then
             begin
               Pdfe.log "Bad /Names array. Name tree will be read as empty\n";
               []
             end
          else pairs_of_list elts
    | _ -> []
  in
    match Pdf.lookup_direct pdf "/Kids" tree with
    | Some (Pdf.Array kids) ->
        names @ flatten (map (read_name_tree pdf) kids)
    | _ -> names

let read_number_tree pdf tree =
  let r = read_name_tree pdf tree in
    try
      map (function (Pdf.Integer i, x) -> (string_of_int i, x) | _ -> raise Exit) r
    with
      Exit ->
        Pdfe.log "Pdfmerge.read_number_tree: skipping malformed name tree\n";
        []

let read_name_tree pdf tree =
  let r = read_name_tree pdf tree in
    try
      map (function (Pdf.String s, x) -> (s, x) | _ -> raise Exit) r
    with
      Exit ->
        Pdfe.log "Pdfmerge.read_name_tree: skipping malformed name tree\n";
        []

let maxsize = 10

type ('k, 'v) nt =
  Br of 'k * ('k, 'v) nt list * 'k
| Lf of 'k * ('k * 'v) list * 'k

let left l = fst (hd l)
let right l = fst (last l)

let rec build_nt_tree l =
  if length l = 0 then assert false;
  if length l <= maxsize
    then Lf (left l, l, right l)
    else Br (left l, map build_nt_tree (splitinto maxsize l), right l)

let rec name_tree_of_nt isnum isroot pdf = function
  Lf (llimit, items, rlimit) ->
    let entry =
      [((if isnum then "/Nums" else "/Names"),
        Pdf.Array (flatten (map (fun (k, v) -> [(if isnum then Pdf.Integer (int_of_string k) else Pdf.String k); v]) items)))] 
    in
    let ll, rl =
      if isnum
        then Pdf.Integer (int_of_string llimit), Pdf.Integer (int_of_string rlimit)
        else Pdf.String llimit, Pdf.String rlimit
    in
      Pdf.Dictionary
        (entry @
         if isroot then [] else [("/Limits", Pdf.Array [ll; rl])])
| Br (llimit, nts, rlimit) ->
    let indirects =
      let kids = map (name_tree_of_nt isnum false pdf) nts in
        map (Pdf.addobj pdf) kids
    in
    let ll, rl =
      if isnum
        then Pdf.Integer (int_of_string llimit), Pdf.Integer (int_of_string rlimit)
        else Pdf.String llimit, Pdf.String rlimit
    in
      Pdf.Dictionary
       [("/Kids", Pdf.Array (map (fun x -> Pdf.Indirect x) indirects));
        ("/Limits", Pdf.Array [ll; rl])]

let compare_any isnum a b =
  if isnum then
    compare (int_of_string (fst a)) (int_of_string (fst b))
  else
    compare a b

let build_name_tree isnum pdf = function
  | [] ->
      if isnum then
        Pdf.Dictionary [("/Nums", Pdf.Array [])]
      else
        Pdf.Dictionary [("/Names", Pdf.Array [])]
  | ls ->
      let nt = build_nt_tree (sort (compare_any isnum) ls) in
        Pdf.remove_dict_entry (name_tree_of_nt isnum true pdf nt) "/Limits"

(* Once we know there are no clashes *)
let merge_name_trees_no_clash pdf trees =
  build_name_tree false pdf (flatten (map (read_name_tree pdf) trees))

let merge_number_trees_no_clash pdf trees =
  build_name_tree true pdf (flatten (map (read_number_tree pdf) trees))
