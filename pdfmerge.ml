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

(* Merge the bookmarks in the pdfs and ranges, adding to the new pdf. changes
is oldpageobjnum, newpageobjnum pairs. *)
let merge_bookmarks changes pdfs ranges pdf =
  try
    let process_mark oldnums changes mark = 
      let pageobjectnumber_of_target = function
        | Pdfdest.NullDestination -> 0
        | Pdfdest.NamedDestinationElsewhere _ -> 0
        | Pdfdest.Action _ -> 0
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
                  | Pdfdest.Action a -> Pdfdest.Action a
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
              flatten (map2 call_process_mark (map Pdfmarks.read_bookmarks pdfs) ranges)
        in
          Pdfmarks.add_bookmarks bookmarks' pdf
  with
    e -> Printf.eprintf "failure in merge_bookmarks %s\n%!" (Printexc.to_string e); pdf

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
      
(* Reading a name tree, flattened. *)
let rec read_name_tree pdf tree =
  let names =
    match Pdf.lookup_direct pdf "/Names" tree with
    | Some (Pdf.Array elts) ->
        if odd (length elts)
          then
             begin
               Printf.eprintf "Bad /Names array. Name tree will be read as empty\n%!";
               []
             end
          else pairs_of_list elts
    | _ -> []
  in
    match Pdf.lookup_direct pdf "/Kids" tree with
    | Some (Pdf.Array kids) ->
        names @ flatten (map (read_name_tree pdf) kids)
    | _ -> names

let read_name_tree pdf tree =
  let r = read_name_tree pdf tree in
    try
      map (function (Pdf.String s, x) -> (s, x) | _ -> raise Exit) r
    with
      Exit ->
        Printf.eprintf "Pdfmerge.read_names tree: skipping malformed name tree\n%!";
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

let rec name_tree_of_nt isroot pdf = function
  Lf (llimit, items, rlimit) ->
    Pdf.Dictionary
      ([("/Names", Pdf.Array (flatten (map (fun (k, v) -> [Pdf.String k; v]) items)))] @
       if isroot then [] else [("/Limits", Pdf.Array [Pdf.String llimit; Pdf.String rlimit])])
| Br (llimit, nts, rlimit) ->
    let indirects =
      let kids = map (name_tree_of_nt false pdf) nts in
        map (Pdf.addobj pdf) kids
    in
      Pdf.Dictionary
       [("/Kids", Pdf.Array (map (fun x -> Pdf.Indirect x) indirects));
        ("/Limits", Pdf.Array [Pdf.String llimit; Pdf.String rlimit])]

let build_name_tree pdf = function
  | [] -> Pdf.Dictionary [("/Names", Pdf.Array [])]
  | ls ->
      let nt = build_nt_tree (sort compare ls) in
        name_tree_of_nt true pdf nt



(* Once we know there are no clashes *)
let merge_name_trees_no_clash pdf trees =
  build_name_tree pdf (flatten (map (read_name_tree pdf) trees))

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

(* Merge name trees This needs to return some changes to be made to Annots when
 * names in the trees might clash. e.g if the name /A appears in two trrees, we
 * might return (6, "/A", "/A-6") to indicate all uses of "/A" in PDF number 6
 * must be rewritten to "/A-6" *)
(* For now, only operates on /Dests, because for now we only know how to find all
 * the uses of these dest names. To merge any of the others properly, we need
 * to find out how to find every instance of their use in the file. We can't
 * just assume any string object is a name tree key *)
(* This runs after merge_pdfs_renumber, so there can be no clashing of values.
 * We can return the OCaml name tree structure safe in the knowledge that it
 * can be written to the eventual merged PDF and the object numbers will be
 * correct. *)
let merge_pdfs_rename_name_trees names pdfs = pdfs
  (* Find the /Dests nametree in each file *)
  (* Calculate the changes *)
  (* Apply the changes to the name tree *)
  (* Apply the changes to each PDFs annots entries and anywhere else Dests can be used. *)
  (* Build our name tree OCaml structure for the merged tree and return. *)

(* FIXME: The problem here is that we may need to break the non-copy of a file
 * multiply included in a merge, since we need to alter its destination
 * objects. It's hard to see a way around this without removing that
 * functionality. See comments in cpdf-source github issue 79. *)

(* Merge catalog items from the PDFs, taking an abitrary instance of any one we
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
    fold_left (fun d (k, v) -> add k v d) [] catalog_entries

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

let merge_pdfs retain_numbering do_remove_duplicate_fonts names pdfs ranges =
  let pdfs = merge_pdfs_renumber names pdfs in
  let pdfs = merge_pdfs_rename_name_trees names pdfs in
    let minor' = fold_left max 0 (map (fun p -> p.Pdf.minor) pdfs) in
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
            in
            let extra_catalog_entries =
              match merge_acroforms pdf pdfs with
              | None -> extra_catalog_entries
              | Some acroformnum -> add "/AcroForm" (Pdf.Indirect acroformnum) extra_catalog_entries
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
   let pdf = {pdf with Pdf.major = 1; Pdf.minor = minor'} in
     let pdf = merge_bookmarks changes pdfs ranges pdf in
       Pdfpagelabels.write pdf page_labels;
       if do_remove_duplicate_fonts then remove_duplicate_fonts pdf;
       Pdf.change_id pdf "";
       pdf
