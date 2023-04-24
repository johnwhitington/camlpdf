open Pdfutil

(* We read all the files, read their pages and concatenate them, dealing with
clashing object numbers. We then build a new page tree, and build the output
PDF document, with a new root and trailer dictionary. We then remove any
unreferenced objects, and write to file.

The [names] argument contains the initial filenames, so that a file referenced
several times (and only loaded once, of course) is not inadvertantly renumbered
to form several separate PDFs, which would increase output size. *)

(* Equality on PDF streams *)
let streameq pdf x y =
  let x = Pdf.lookup_obj pdf x 
  and y = Pdf.lookup_obj pdf y in
    Pdf.getstream x;
    Pdf.getstream y;
    match x, y with
    | Pdf.Stream {contents = (dict, Pdf.Got (bytes))},
      Pdf.Stream {contents = (dict', Pdf.Got (bytes'))} ->
        compare (dict, bytes) (dict', bytes')
    | _ -> raise (Pdf.PDFError "streameq")

let remove_duplicate_fonts pdf =
  let streamobjs = ref [] in
    Pdf.objiter
      (fun objnum obj ->
         match obj with Pdf.Stream _ -> streamobjs := objnum::!streamobjs | _ -> ())
      pdf;
    let toprocess =
      keep (fun x -> length x > 1) (collate (streameq pdf) (sort (streameq pdf) !streamobjs))
    in
      let pdfr = ref pdf in
        iter
          (function [] -> assert false | h::t ->
             let changetable = Hashtbl.create 100 in
               iter (fun e -> Hashtbl.add changetable e h) t;
               pdfr := Pdf.renumber changetable !pdfr)
          toprocess;
        pdf.Pdf.root <- !pdfr.Pdf.root;
        pdf.Pdf.objects <- !pdfr.Pdf.objects;
        pdf.Pdf.trailerdict <- !pdfr.Pdf.trailerdict

let debug_pagelabels ls =
  iter (Printf.printf "%s\n") (map Pdfpagelabels.string_of_pagelabel ls)

let debug_collection_of_pagelabels =
  iter (fun ls -> debug_pagelabels ls; flprint "\n")

(* Merging /Dests (Named destinations) in the catalog (PDF v1.1 style, rather
 * than in the PDF 1.3 style in the name tree). Since the new /Dests must be an
 * indirect reference, we add the new object to the pdf, returning the new pdf
 * and the reference.
FIXME: merging a v1.1 file with a v1.2 file will result in both sets of dests, confusing the reader...*)
let new_dests pdf pdfs =
  let dests =
    option_map
      (function pdf ->
        let catalog = Pdf.catalog_of_pdf pdf in
          match Pdf.lookup_direct pdf "/Dests" catalog with
          | Some (Pdf.Dictionary d) -> Some d
          | _ -> None)
      pdfs
  in
    if dests = [] then None else
      let new_dests =
        Pdf.Dictionary (flatten dests)
      in
        Some (Pdf.addobj pdf new_dests)

(* Names distinguish PDFs which are actually the same. So we only use the first
of each group of same ones. Then renumber them. and return. *)
let merge_pdfs_renumber names pdfs =  
  (* Can't use setify due to functional values in PDFs, so use a hash table *)
  let h = Hashtbl.create 20 in
    iter2 (Hashtbl.replace h) names pdfs;
    let first_names, first_pdfs =
      let ns = ref [] and ps = ref [] in
        Hashtbl.iter
          (fun name pdf ->
          ns := name::!ns; ps := pdf::!ps) h;
        (!ns, !ps)
      in
        let table = combine first_names (Pdf.renumber_pdfs first_pdfs) in
          map (function k -> lookup_failnull k table) names
      
(* Reading a name tree, flattened. FIXME: This appears to be somewhat duplicated in Pdf.ml.
   Move name / number tree stuff to pdftree.ml *)
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

let maxsize = 10 (* Must be at least two *)

type ('k, 'v) nt =
  Br of 'k * ('k, 'v) nt list * 'k
| Lf of 'k * ('k * 'v) list * 'k

let left l = fst (hd l)
let right l = fst (last l)

let rec build_nt_tree l =
  if length l = 0 then assert false;
  if length l <= maxsize
    then Lf (left l, l, right l)
    else Br (left l, map build_nt_tree (splitinto (length l / maxsize) l), right l)

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

let build_name_tree isnum pdf = function
  | [] ->
      if isnum then
        Pdf.Dictionary [("/Nums", Pdf.Array [])]
      else
        Pdf.Dictionary [("/Names", Pdf.Array [])]
  | ls ->
      let nt = build_nt_tree (sort compare ls) in
        name_tree_of_nt isnum true pdf nt

(* Once we know there are no clashes *)
let merge_name_trees_no_clash pdf trees =
  build_name_tree false pdf (flatten (map (read_name_tree pdf) trees))

let merge_number_trees_no_clash pdf trees =
  build_name_tree true pdf (flatten (map (read_number_tree pdf) trees))

(* Merging entries in the Name Dictionary. [pdf] here is the new merged pdf, [pdfs] the original ones. *)
let merge_namedicts pdf pdfs =
  let names =
    ["/Dests"; "/AP"; "/JavaScript"; "/Pages"; "/Templates"; "/IDS";
     "/URLS"; "/EmbeddedFiles"; "/AlternatePresentations"; "/Renditions"]
  in
    let gettree name pdf =
      let catalog = Pdf.catalog_of_pdf pdf in
        match Pdf.lookup_direct pdf "/Names" catalog with
        | Some d -> Pdf.lookup_direct pdf name d
        | None -> None
    in
    (* Build a list of the lists of trees for each name *)
    let trees_in_each =
      map (fun name -> option_map (gettree name) pdfs) names
    in
      (* Combine with the names, and lose nulls *)
      let with_names =
        lose
          (function (_, []) -> true | _ -> false)
          (combine names trees_in_each)
      in
        let new_trees =
          map
            (fun (name, trees) -> name, merge_name_trees_no_clash pdf trees)
            with_names
        in
          (* Add all the trees as indirect references to the pdf *)
          let nums = ref [] in
            iter
              (function (_, obj) ->
                let num = Pdf.addobj pdf obj in
                  nums =| num)
              new_trees;
          (* Build the new name dictionary *)
          let newdict =
            Pdf.Dictionary
              (map2
                (fun (name, _) n -> name, Pdf.Indirect n)
                new_trees
                (rev !nums))
          in
            (* Return the new pdf, and the new dictionary. *)
            Pdf.addobj pdf newdict

(* Merge the bookmarks in the pdfs and ranges, adding to the new pdf. changes
is oldpageobjnum, newpageobjnum pairs. *)
let merge_bookmarks changes pdfs ranges pdf =
  (*Printf.printf "changes:\n";
  iter (fun (a, b) -> Printf.printf "%i -> %i\n" a b) changes;*)
  try
    let dest_nametree =
      let catalog = Pdf.catalog_of_pdf pdf in
        let oldstyle =
          match Pdf.lookup_direct pdf "/Dests" catalog with
          | Some d -> read_name_tree pdf d
          | _ -> []
        in
        let newstyle =
          match Pdf.lookup_direct pdf "/Names" catalog with
          | Some d ->
              begin match Pdf.lookup_direct pdf "/Dests" d with
              | Some d -> read_name_tree pdf d
              | _ -> []
              end
          | _ -> []
        in
          oldstyle @ newstyle
    in
    let process_mark oldnums changes mark = 
      let rec pageobjectnumber_of_target t =
        let pagenumber_of_target_string s =
          (*Printf.printf "merge_bookmarks: found string %s in action\n" s;*)
          (* Look up in /Dest name tree. Then /D is the destionation. Read it and recurse. *)
          begin match lookup s dest_nametree with
          | Some dest ->
              begin match Pdf.lookup_direct pdf "/D" dest with
              | Some dest ->
                  let r = pageobjectnumber_of_target (Pdfdest.read_destination pdf dest) in
                    (*Printf.printf "Is new page object number %i\n" r;*)
                    begin match lookup r (map (fun (x, y) -> (y, x)) changes) with
                    | Some old -> (*Printf.printf "Which was old page obj number %i\n" old;*) old
                    | None -> r
                    end
              | None -> 0
              end
          | None -> 0
          end
        in
        match t with
        | Pdfdest.NullDestination -> 0
        | Pdfdest.NamedDestinationElsewhere s -> pagenumber_of_target_string s 
        | Pdfdest.Action a ->
            (* Look for a /GoTo and find the page number. If /S /GoTo then read /D destination string.
            By the time this is called, we have the new merged PDF name tree done, so this should
            all be correct. *)
            begin match Pdf.lookup_direct pdf "/S" a with
            | Some (Pdf.Name "/GoTo") ->
                begin match Pdf.lookup_direct pdf "/D" a with
                | Some (Pdf.String s) -> pagenumber_of_target_string s 
                | _ -> 0
                end
            | _ -> 0
            end
        | Pdfdest.XYZ (t, _, _, _) | Pdfdest.Fit t | Pdfdest.FitH (t, _) | Pdfdest.FitV (t, _)
        | Pdfdest.FitR (t, _, _, _, _) | Pdfdest.FitB t | Pdfdest.FitBH (t, _) | Pdfdest.FitBV (t, _) ->
            match t with
            | Pdfdest.OtherDocPageNumber _ -> 0
            | Pdfdest.PageObject i -> i
      in
        let objnum = pageobjectnumber_of_target mark.Pdfmarks.target in
          (*Printf.printf "Considering objnum %i for inclusion...\n" objnum;*)
          if mem objnum oldnums || mark.Pdfmarks.target = Pdfdest.NullDestination (* If this bookmark is to be included... *)
            then
              let change_target_destinationpage target n =
                let change_targetpage = function
                  | Pdfdest.OtherDocPageNumber a -> Pdfdest.OtherDocPageNumber a
                  | Pdfdest.PageObject _ -> Pdfdest.PageObject n 
                in
                  match target with
                  | Pdfdest.Action a -> 
                      (* The target page has already been updated. *)
                      Pdfdest.Action a
                  | Pdfdest.NullDestination -> Pdfdest.NullDestination
                  | Pdfdest.NamedDestinationElsewhere s -> Pdfdest.NamedDestinationElsewhere s
                  | Pdfdest.XYZ (t, a, b, c) -> Pdfdest.XYZ (change_targetpage t, a, b, c)
                  | Pdfdest.Fit t -> Pdfdest.Fit (change_targetpage t)
                  | Pdfdest.FitH (t, a) -> Pdfdest.FitH (change_targetpage t, a)
                  | Pdfdest.FitV (t, a) -> Pdfdest.FitV (change_targetpage t, a)
                  | Pdfdest.FitR (t, a, b, c, d) -> Pdfdest.FitR (change_targetpage t, a, b, c, d)
                  | Pdfdest.FitB t -> Pdfdest.FitB (change_targetpage t)
                  | Pdfdest.FitBH (t, a) -> Pdfdest.FitBH (change_targetpage t, a)
                  | Pdfdest.FitBV (t, a) -> Pdfdest.FitBV (change_targetpage t, a)
              in
                Some
                  {mark with Pdfmarks.target =
                     if mark.Pdfmarks.target = Pdfdest.NullDestination
                       then Pdfdest.NullDestination
                       else change_target_destinationpage mark.Pdfmarks.target (lookup_failnull objnum changes)}
           else
             None
      in
        let bookmarks' =
          let oldnums = ref (fst (split changes))
          and changes = ref changes in
            let call_process_mark marks range =
              let r =
                (* Pass just the oldnums / changes in question. This is a fix
                for when a single file is multiply included without renumbering
                for efficiency. *)
                option_map
                  (process_mark (take !oldnums (length range)) (take !changes (length range)))
                  marks
              in
                (* Remove (length range) things from the beginning of !oldnums
                / !changes.  This allows the function to work properly when a
                single file is included unrenumbered multiple times due to
                being included twice or more in the merge! *)
                oldnums := drop !oldnums (length range);
                changes := drop !changes (length range);
                r
            in
              let markss = map (Pdfmarks.read_bookmarks ~preserve_actions:true) pdfs in
                (*iter
                  (fun l -> Printf.printf "\n%i marks\n:" (length l); iter (fun m -> Printf.printf "%s\n" (Pdfmarks.string_of_bookmark m)) l)
                  markss;*) 
                flatten (map2 call_process_mark markss ranges)
        in
          Pdfmarks.add_bookmarks bookmarks' pdf
  with
    e -> Pdfe.log (Printf.sprintf "failure in merge_bookmarks %s\n" (Printexc.to_string e)); pdf

(* This is a pre-processing step to deduplicate name trees. It presently only
   runs on destination name trees, because that's the only kind where we know
   how to find all uses of these names (can't just assume any identical string
   in a PDF is a name). In future, it will be expanded to the other name trees.

   This runs after merge_pdfs_renumber, so there can be no clashing of values.
   We can return the OCaml name tree structure safe in the knowledge that it
   can be written to the eventual merged PDF and the object numbers will be
   correct.

   FIXME: This is insufficient in one weird case: if a file has some or all
   pages multiply included, the names will not be disabiguated between the uses:
   e.g cpdf -merge a.pdf b.pdf a.pdf -o out.pdf
   links in the second a.pdf here would point to destinations in the first a.pdf.
   The workaround here is to simply rename the file so it appears to be two files
   before processing.
   *)
let apply_namechanges_to_destination_nametree pdf changes =
  let changes = hashtable_of_dictionary changes in
  let rewrite_string s =
    try (*let r = *) Hashtbl.find changes s (*in Printf.printf "%s -> %s\n" s r; r*) with
      Not_found -> Pdfe.log ("apply_namechanges_to_destination_nametree: destination not found: " ^ s); s
  in
  let rec rewrite_kids d =
    (*Printf.printf "rewrite_kids on dict %s\n" (Pdfwrite.string_of_pdf d);*)
    match Pdf.lookup_direct pdf "/Kids" d with
    | Some (Pdf.Array is) ->
        iter (function Pdf.Indirect i -> Pdf.addobj_given_num pdf (i, rewrite (Pdf.direct pdf (Pdf.Indirect i))) | _ -> ()) is;
    | _ -> ()
  and rewrite dict =
    (*Printf.printf "rewrite_dict on %s\n" (Pdfwrite.string_of_pdf dict);*)
    (* 1. Update the names per the change, if a change is found. *)
    let dict =
      match Pdf.lookup_direct pdf "/Names" dict with
      | Some (Pdf.Array a) ->
          begin try
            let pairs = map (function (Pdf.String x, o) -> (Pdf.String (rewrite_string x), o) | x -> x) (pairs_of_list a) in
            let a' = flatten (map (fun (a, b) -> [a; b]) pairs) in
              Pdf.add_dict_entry dict "/Names" (Pdf.Array a')
          with
            Invalid_argument _ -> Pdfe.log "Warning: malformed /Names"; dict
          end
      | _ -> dict
    in
    let dict = 
      match Pdf.lookup_direct pdf "/Limits" dict with
      | Some (Pdf.Array [Pdf.String a; Pdf.String b]) ->
          Pdf.add_dict_entry dict "/Limits" (Pdf.Array [Pdf.String (rewrite_string a); Pdf.String (rewrite_string b)])
      | _ -> dict
    in
      rewrite_kids dict;
      dict
  in
  (* Find dest name tree root /Root -> /Names -> /Dests. *)
  let catalog = Pdf.catalog_of_pdf pdf in
    match Pdf.lookup_direct pdf "/Names" catalog with
    | Some (Pdf.Dictionary d) -> 
        begin match lookup "/Dests" d with
        | Some (Pdf.Dictionary d) ->
            (* If direct, update it in place, and rewrite the kids *)
            rewrite_kids (Pdf.Dictionary d);
            let newdest = rewrite (Pdf.Dictionary d) in
            let newcatalog = Pdf.add_dict_entry catalog "/Dests" newdest in
            Pdf.addobj_given_num pdf (pdf.Pdf.root, newcatalog)
        | Some (Pdf.Indirect i) ->
            (* If indirect, just begin *)
            Pdf.addobj_given_num pdf (i, rewrite (Pdf.direct pdf (Pdf.Indirect i)))
        | _ -> ()
        end
    | _ -> ()

let apply_namechanges_at_destination_callsites pdf changes =
  let changes = hashtable_of_dictionary changes in
  let rewrite_string s =
    try Hashtbl.find changes s with
      Not_found -> Pdfe.log ("warning: apply_namechanges_at_destination_callsite: destination not found: " ^ s); s
  in
    let rec f = function
    | Pdf.Dictionary d ->
        let d = Pdf.recurse_dict f d in
        (* Rewrite any /S /GoTo (/D) *)
        let d =
          begin match Pdf.lookup_direct pdf "/S" d with
          | Some (Pdf.Name "/GoTo") ->
              begin match Pdf.lookup_direct pdf "/D" d with
              | Some (Pdf.String s) ->
                  Pdf.add_dict_entry d "/D" (Pdf.String (rewrite_string s))
              | _ -> d
              end
          | _ -> d
          end;
        in
          (* Rewrite any /Dest *)
          begin match Pdf.lookup_direct pdf "/Dest" d with
          | Some (Pdf.String s) ->
              Pdf.add_dict_entry d "/Dest" (Pdf.String (rewrite_string s))
          | _ -> d
          end
    | Pdf.Array a -> Pdf.recurse_array f a
    | x -> x
    in
    Pdf.objselfmap f pdf;
    (* Include trailerdict for /OpenAction *)
    pdf.Pdf.trailerdict <- f pdf.Pdf.trailerdict

let mdebug = ref false

let merge_pdfs_rename_name_trees names pdfs =
  (* Find unique PDFs, based on names arg. *)
  let cmp (a, _) (b, _) = compare a b in
  let pdfs = map snd (map hd (collate cmp (sort cmp (combine names pdfs)))) in
  if !mdebug then Printf.printf "merge_pdfs_rename_name_trees %i pdfs\n" (length pdfs);
  (* Find the /Dests nametree in each file. /Root -> /Names -> /Dests. *)
  let pdfs_and_nametrees =
    map
      (function pdf ->
        let catalog = Pdf.catalog_of_pdf pdf in
          match Pdf.lookup_direct pdf "/Names" catalog with
          | Some d -> 
              begin match Pdf.lookup_direct pdf "/Dests" d with
              | Some (Pdf.Dictionary d) -> (pdf, Some (Pdf.Dictionary d))
              | _ -> (pdf, None)
              end
          | _ -> (pdf, None))
      pdfs
  in
  (* Read the list of names *)
  let names =
    map
      (function
       | (pdf, Some nametree) -> (pdf, map fst (read_name_tree pdf nametree))
       | (pdf, None) -> (pdf, []))
      pdfs_and_nametrees
  in
  if !mdebug then iter (fun (_, ns) -> iter (fun n -> Printf.printf "%s\n" n) ns; Printf.printf "\n") names;
  (* Calculate the changes e.g (pdf, "section", "section.f1") *)
  let num = ref ~-1 in
  let worked l =
    let ns = flatten (map (fun (pdf, ns) -> map snd ns) l) in
      length (setify ns) = length ns
  in
  let names = ref (map (fun (pdf, ns) -> (pdf, map (fun n -> (n, n)) ns)) names) in
  while not (worked !names) do
    num += 1;
    names :=
      map2
        (fun (pdf, ns) i ->
           (pdf, map (fun (n, _) -> if i = 0 then (n, n) else (n, n ^ "-f" ^ string_of_int i)) ns))
        !names
        (ilist !num (!num + length !names - 1))
  done;
  if !mdebug then
    begin
      Printf.printf "\nAfter changes\n";
      iter (fun (pdf, ns) -> iter (fun (nold, nnew) -> Printf.printf "%s %s \n" nold nnew) ns) !names;
    end;
  (* 2. Remove any names which don't change. *)
  let tochange =
    option_map
      (fun (pdf, ns) -> let ns' = keep (fun (n, n') -> n <> n') ns in if ns' = [] then None else Some (pdf, ns'))
      !names
  in
  if !mdebug then Printf.printf "%i pdfs to fix up\n" (length tochange);
  (* Apply the changes to the destination name tree in each file, in place *)
  iter (fun (pdf, changes) -> apply_namechanges_to_destination_nametree pdf changes) tochange;
  (* Apply the changes to each PDFs annots entries and anywhere else Dests can be used, in place. *)
  iter (fun (pdf, changes) -> apply_namechanges_at_destination_callsites pdf changes) tochange

(* Merge catalog items from the PDFs, taking the first instance of any one we
 * find. Items we know how to merge properly, like /Dests, /Names, /PageLabels,
 * /Outlines, will be overwritten, so we don't worry about them here. *)
let catalog_items_from_original_documents pdfs =
  let catalog_entries =
    flatten
      (map
        (fun pdf ->
           match Pdf.catalog_of_pdf pdf with
             Pdf.Dictionary es -> es
           | _ -> failwith "catalog_items_from_original_documents")
        pdfs)
  in
    fold_right (fun (k, v) d -> add k v d) catalog_entries []

(* Optional Content Groups. We merge the /OCProperties entries like this:
  * a) append /OCGs entries
  * b) append /Configs entries, or leave absent if everywhere-absent.
  * c) combine /D dictionary by appending its subentries:
    *    1) /ON / /OFF arrays, merge or absent if everywhere-absent
    *    2) /Order ditto
    *    3) /RBGroups ditto
    *    4) Use first or any found /Locked /ListMode /AS /Intent /BaseState /Creator /Name *)
let merge_entries pdf n maindict =
  flatten
    (option_map
       (fun dict -> match Pdf.lookup_direct pdf n dict with Some (Pdf.Array a) -> Some a | _ -> None)
       maindict)

let merge_default_dictionaries pdf dics =
  let merge_empty_is_none n =
    match option_map (Pdf.lookup_direct pdf n) dics with
    | [] -> []
    | l -> [(n, Pdf.Array (flatten (option_map (function Pdf.Array a -> Some a | _ -> None) l)))]
  in
    let merged_on = merge_empty_is_none "/ON" in
    let merged_off = merge_empty_is_none "/OFF" in
    let merged_order = merge_empty_is_none "/Order" in
    let merged_rbgroups = merge_empty_is_none "/RBGroups" in
    let simple_copies =
      let keys =
        ["/Locked"; "/ListMode"; "/AS"; "/Intent"; "/BaseState"; "/Creator"; "/Name"]
      in
      let find_first_item key =
        match option_map (Pdf.lookup_direct pdf key) dics with
        | [] -> []
        | h::_ -> [(key, h)]
      in
        flatten (map find_first_item keys)
    in
      (*Printf.printf "merged_on length %i\n" (length merged_on);
      Printf.printf "merged_off length %i\n" (length merged_off);
      Printf.printf "merged_order length %i\n" (length merged_order);
      Printf.printf "merged_rbgroups length %i\n" (length merged_rbgroups);
      Printf.printf "simple_copies length %i\n" (length simple_copies);*)
      merged_on @ merged_off @ merged_order @ merged_rbgroups @ simple_copies

let merge_optional_content_groups pdf pdfs =
  let ocp_dicts =
    option_map
      (fun pdf -> Pdf.lookup_direct pdf "/OCProperties" (Pdf.catalog_of_pdf pdf))
      pdfs
  in
    if ocp_dicts = [] then None else
    let merged_ocg_arrays = merge_entries pdf "/OCGs" ocp_dicts in
    let merged_config_arrays = merge_entries pdf "/Configs" ocp_dicts in
    let merged_default_dictionary =
      merge_default_dictionaries pdf (option_map (Pdf.lookup_direct pdf "/D") ocp_dicts)
    in
    let new_ocproperties =
      Pdf.Dictionary
        ([("/OCGs", Pdf.Array merged_ocg_arrays); ("/D", Pdf.Dictionary merged_default_dictionary)]
         @ if merged_config_arrays = [] then [] else [("/Configs",  Pdf.Array merged_config_arrays)])
    in
      Some (Pdf.addobj pdf new_ocproperties)

(* Look up any acroforms and merge their fields entries, retaining any
   other entries from any found. This is very basic, and we need to
   see more example files to know what to do properly. *)
let merge_acroforms pdf pdfs =
  let form_dicts =
    option_map
      (fun pdf -> Pdf.lookup_direct pdf "/AcroForm" (Pdf.catalog_of_pdf pdf))
      pdfs
  in
    if form_dicts = [] then None else
    let merged_field_arrays =
      flatten
        (map
          (fun d -> match Pdf.lookup_direct pdf "/Fields" d with Some (Pdf.Array a) -> a | _ -> [])
          form_dicts)
    in
    let merged_nonfield_items =
      fold_left
        (fun d (k, v) -> Pdf.add_dict_entry d k v)
        (Pdf.Dictionary [])
        (flatten (map (function (Pdf.Dictionary d) -> d | _ -> []) form_dicts))
    in
    let new_dict =
      Pdf.add_dict_entry merged_nonfield_items "/Fields" (Pdf.Array merged_field_arrays)
    in
      Some (Pdf.addobj pdf new_dict)

(* Merge structure heirarchy / tagged PDF.

/IDTree                 name tree     merge
/ParentTree             number tree   merge
/ParentTreeNextKey      integer       delete for now, since optional and needs updating
/RoleMap                dict          merge
/ClassMap               dict          merge
/NameSpaces             array         merge
/PronunciationLexicon   array         merge
/AF                     array         merge
/K                      dict/array    merge
*)
let merge_structure_hierarchy pdf pdfs =
  let get_all struct_tree_roots pdf name =
    option_map
      (fun str -> Pdf.lookup_direct pdf name str) struct_tree_roots
  in
  let merge_dicts dicts =
    fold_left
      (fun d (k, v) -> Pdf.add_dict_entry d k v)
      (Pdf.Dictionary [])
      (flatten
        (option_map
          (function
           | Pdf.Dictionary d -> Some d
           | _ -> Pdfe.log "merge_dicts: not a dict"; None) dicts))
  in
  let merge_arrays arrays =
    Pdf.Array
      (flatten
        (option_map
          (function
           | Pdf.Array a -> Some a
           | _ -> Pdfe.log "merge_array: not an array"; None) arrays))
  in
  let mkarray = function
    | Pdf.Array a -> Pdf.Array a
    | x -> Pdf.Array [x]
  in
  let struct_tree_roots, struct_tree_objnums =
    split
      (option_map
        (fun pdf ->
          let catalog = Pdf.catalog_of_pdf pdf in
             match Pdf.lookup_direct pdf "/StructTreeRoot" catalog with
             | None -> None
             | Some str ->
                Some
                  (str,
                   match catalog with
                   | Pdf.Dictionary d ->
                       begin match lookup "/StructTreeRoot" d with Some (Pdf.Indirect i) -> i | _ -> 0 end
                   | _ -> raise (Pdf.PDFError "merge_structure_hierarchy: bad catalog")))
        pdfs)
  in
    match struct_tree_roots with
    | [] -> None
    | [x] ->
       Some (hd struct_tree_objnums) (* if only one, don't interfere, just preserve it. *)
    | _ ->
      let merged_idtree =
        merge_name_trees_no_clash pdf (get_all struct_tree_roots pdf "/IDTree")
      in
      let merged_parenttree =
        merge_number_trees_no_clash pdf (get_all struct_tree_roots pdf "/ParentTree")
      in
      let merged_rolemap =
        merge_dicts (get_all struct_tree_roots pdf "/RoleMap") in
      let merged_classmap =
        merge_dicts (get_all struct_tree_roots pdf "/ClassMap") in
      let merged_namespaces =
        merge_arrays (get_all struct_tree_roots pdf "/NameSpaces") in
      let merged_pronunciation_lexicon =
        merge_arrays (get_all struct_tree_roots pdf "/PronunciationLexicon") in
      let merged_af =
        merge_arrays (get_all struct_tree_roots pdf "/AF") in
      let struct_tree_objnum = Pdf.addobj pdf Pdf.Null in
      let merged_k =
        match merge_arrays (map mkarray (get_all struct_tree_roots pdf "/K")) with
        | Pdf.Array l ->
            Pdf.Array (map (fun d -> Pdf.add_dict_entry d "/P" (Pdf.Indirect struct_tree_objnum)) (map (Pdf.direct pdf) l))
        | _ -> assert false
      in
        let optional n = function
        | Pdf.Dictionary [] -> []
        | Pdf.Array [] -> []
        | x -> [(n, Pdf.Indirect (Pdf.addobj pdf x))]
        in
        let new_dict =
          Pdf.Dictionary
            (["/Type", Pdf.Name "/StructTreeRoot"]
             @ optional "/IDTree" merged_idtree
             @ optional "/ParentTree" merged_parenttree
             @ optional "/RoleMap" merged_rolemap
             @ optional "/ClassMap" merged_classmap
             @ optional "/NameSpaces" merged_namespaces
             @ optional "/PronunciationLexicon" merged_pronunciation_lexicon
             @ optional "/AF" merged_af
             @ optional "/K" merged_k)
        in
          Pdf.addobj_given_num pdf (struct_tree_objnum, new_dict);
          Some struct_tree_objnum

let max_version_number pdfs =
  if pdfs = [] then raise (Invalid_argument "max_version_number") else
    hd (sort compare (map (fun p -> (p.Pdf.major, p.Pdf.minor)) pdfs))

let merge_pdfs retain_numbering do_remove_duplicate_fonts names pdfs ranges =
  let pdfs = merge_pdfs_renumber names pdfs in
    merge_pdfs_rename_name_trees names pdfs;
    let major', minor' = max_version_number pdfs in
      let pagelists = map Pdfpage.pages_of_pagetree pdfs
      in let pdf = Pdf.empty () in
        let select_pages range pagelist =
          let pages = ref [] in
            iter (fun n -> pages =| select n pagelist) range;
            rev !pages
        in
  let pages = flatten (map2 select_pages ranges pagelists) in
    iter (Pdf.objiter (fun k v -> ignore (Pdf.addobj_given_num pdf (k, v)))) pdfs;
    let pdf, pagetree_num = Pdfpage.add_pagetree pages pdf in
      let page_labels =
        if retain_numbering
         then Pdfpagelabels.merge_pagelabels pdfs ranges
         else []
      in
        let dests = new_dests pdf pdfs in
          let namedict = merge_namedicts pdf pdfs in
            let extra_catalog_entries =
              let with_names =
                (add  "/Names" (Pdf.Indirect namedict)
                  (catalog_items_from_original_documents pdfs))
              in
                match dests with
                  None -> with_names
                | Some dests ->
                    add "/Dests" (Pdf.Indirect dests) with_names
            in
            (* Merge Optional content groups *)
            let extra_catalog_entries =
              match merge_optional_content_groups pdf pdfs with
                None -> extra_catalog_entries
              | Some ocgpropnum -> add "/OCProperties" (Pdf.Indirect ocgpropnum) extra_catalog_entries
              | exception e ->
                  Pdfe.log (Printf.sprintf "Warning: failed to merge OCGs (%s)\n" (Printexc.to_string e));
                  extra_catalog_entries
            in
            let extra_catalog_entries =
              match merge_acroforms pdf pdfs with
              | None -> extra_catalog_entries
              | Some acroformnum -> add "/AcroForm" (Pdf.Indirect acroformnum) extra_catalog_entries
              | exception e ->
                  Pdfe.log (Printf.sprintf "Warning: failed to merge Acroforms (%s)\n" (Printexc.to_string e));
                  extra_catalog_entries
            in
            let extra_catalog_entries =
              match merge_structure_hierarchy pdf pdfs with
              | None -> extra_catalog_entries
              | exception e ->
                  Pdfe.log (Printf.sprintf "Warning: failed to merge structure tree roots (%s)\n" (Printexc.to_string e));
                  extra_catalog_entries
              | Some structheirnum -> add "/StructTreeRoot" (Pdf.Indirect structheirnum) extra_catalog_entries
            in
   let extra_catalog_entries = remove "/OpenAction" extra_catalog_entries in
   let infodict = Pdf.lookup_direct (hd pdfs) "/Info" (hd pdfs).Pdf.trailerdict in
   let pdf =
     match infodict with
     | None -> pdf
     | Some infodict ->
       {pdf with Pdf.trailerdict = Pdf.Dictionary ["/Info", Pdf.Indirect (Pdf.addobj pdf infodict)]}
   in
   let pdf = Pdfpage.add_root pagetree_num extra_catalog_entries pdf in
      (* To sort out annotations etc. *)
      let old_page_numbers =
        let select_page_numbers range pageobjnums =
          let pages = ref [] in
            iter (fun n -> pages =| select n pageobjnums) range;
            rev !pages
        in
          flatten (map2 select_page_numbers ranges (map Pdf.page_reference_numbers pdfs))
      in let new_page_numbers =
        Pdf.page_reference_numbers pdf
      in
        let changes = combine old_page_numbers new_page_numbers in
          Pdf.objselfmap
          (Pdf.renumber_object_parsed pdf (hashtable_of_dictionary changes))
          pdf;
   let pdf = {pdf with Pdf.major = major'; Pdf.minor = minor'} in
     let pdf = merge_bookmarks changes pdfs ranges pdf in
       Pdfpagelabels.write pdf page_labels;
       if do_remove_duplicate_fonts then remove_duplicate_fonts pdf;
       Pdf.change_id pdf "";
       pdf
