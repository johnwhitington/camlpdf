(** Operations on structure trees. *)
open Pdfutil

(* NB. This is very tightly integrated into pdfmerge.ml/pdfpage.ml with all
   sorts of phase-order intricacies. Beware. *)

(* Future structure tree merging/trimming work:
   
   o Stamping with -stamp-on, -stamp-under, -combine-pages needs to renumber
   MCIDs on each page in some way.

   o What should we do with IDTree, RoleMap, ClassMap, NameSpaces? Simple
   mechanisms work for all known examples, but known examples are few.

   o Do we need to resurrect nulling of references to deleted annots when
   trimming, for example, so they don't accidentally get included due to being
   referenced from the parent tree? This is not a correctness issue, but a
   space one. *)

(* Recursion between modules *)
let endpage = ref (fun _ -> 0)

(* Remove any structure tree node (and therefore its children) which has a page
   number pointing to a page not to be included in the output. This should be a
   reasonable first approximation to the required behaviour. Pdfpage.pdf_of_pages
   immediately after making the copied PDF. *)
let trim_structure_tree pdf range =
  let page_objnums_to_remove =
    let objnums = Pdf.page_reference_numbers pdf in
      map (fun x -> List.nth objnums (x - 1)) (setminus (ilist 1 (!endpage pdf)) range)
  in
    (* Calculate initial deletions - any object with /Pg not in range. *)
    let del = ref [] in
      Pdf.objiter 
        (fun n o ->
           match Pdf.direct pdf o with
           | Pdf.Dictionary d ->
               begin match List.assoc_opt "/Pg" d with
               | Some (Pdf.Indirect i) ->
                   if mem i page_objnums_to_remove then del := n::!del
               | _ -> ()
               end
           | _ -> ())
        pdf;
      (* Any /K referencing these deleted objects is modifed to no longer reference it. *)
      let replaceobjs = ref [] in
      while !del <> [] || !replaceobjs <> [] do
        (*Printf.printf "Top of loop. %i to remove, %i to replace\n" (length (setify_large !del)) (length (setify_large !replaceobjs));
        iter (fun x -> Printf.printf "Removing %s\n" (Pdfwrite.string_of_pdf (Pdf.lookup_obj pdf x))) (setify_large !del);*)
        iter (Pdf.removeobj pdf) (setify_large !del);
        del := [];
        (*iter
          (fun (x, y) -> Printf.printf "Replacing %s with\n   %s\n" x y)
          (map (fun (n, r) -> (Pdfwrite.string_of_pdf (Pdf.lookup_obj pdf n), Pdfwrite.string_of_pdf r)) (setify_large !replaceobjs));*)
        iter (Pdf.addobj_given_num pdf) (setify_large !replaceobjs);
        replaceobjs := [];
        Pdf.objiter
          (fun n o ->
             let process objs = 
               let survives = function
                 | Pdf.Indirect i ->
                     (* Must a) still exist in this file *)
                     Pdf.lookup_obj pdf i <> Pdf.Null  &&
                     (* b) not be an object reference dictionary or marked content reference dictionary referencing a /Pg which is to be deleted *)
                     (* c) not be an object reference dictionary with a /Obj which is a deleted page *)
                     begin match Pdf.indirect_number pdf "/Pg" (Pdf.Indirect i), Pdf.indirect_number pdf "/Obj" (Pdf.Indirect i) with
                     | Some i, _ when mem i page_objnums_to_remove -> false
                     | _, Some i when mem i page_objnums_to_remove -> false
                     | _ -> true
                     end
                 | _ -> true
               in
                 if List.for_all survives objs then None else Some (keep survives objs)
             in
             let process_indirect d is =
               begin match process is with
               | None -> () (* no change *)
               | Some [] ->
                   (* empty not allowed - we must now delete this object *)
                   del := n::!del
               | Some newlist ->
                   (* update the value of /K and change the object in place *)
                   begin match newlist with
                   | [e] -> replaceobjs =| (n, Pdf.replace_dict_entry (Pdf.Dictionary d) "/K" e)
                   | e::es -> replaceobjs =| (n, Pdf.replace_dict_entry (Pdf.Dictionary d) "/K" (Pdf.Array (e::es)))
                   | _ -> assert false
                   end
               end
             in
               match Pdf.direct pdf o with
               | Pdf.Dictionary d ->
                   begin match List.assoc_opt "/K" d with
                   | Some (Pdf.Integer _) -> ()
                   | Some (Pdf.Indirect i) -> process_indirect d [Pdf.Indirect i]
                   | Some (Pdf.Array objs) -> process_indirect d objs
                   | _ -> () (* /K can exist in transparency object, but is a boolean so ok. *)
                   end
             | _ -> ())
          pdf
        done

(* Merge structure hierarchy / tagged PDF. Asterisked items will require
   further work when we find good examples.

/IDTree                 name tree       *merge
/ParentTree             number tree     renumber and merge
/ParentTreeNextKey      integer         remove
/RoleMap                dict            *merge
/ClassMap               dict            *merge
/NameSpaces             array           *merge
/PronunciationLexicon   array           concatenate
/AF                     array           concatenate
/K                      structure tree  merge trees *)

let print_parent_tree = 
  iter (fun (a, b) -> Printf.printf "%s -> %s\n" a (Pdfwrite.string_of_pdf b))

let renumber_parent_trees pdfs =
  if length pdfs = 1 then () else
  let parent_trees =
    map
      (fun pdf ->
         match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/StructTreeRoot"; "/ParentTree"] with
         | Some t -> Pdftree.read_number_tree pdf t
         | None -> [])
    pdfs
  in
  (* iter2 (fun pt n -> Printf.printf "****************** PARENT TREE %i:\n" n; print_parent_tree pt) parent_trees (ilist 1 (length pdfs)); *)
  (* Calculate a renumbering mapping from (pdf number, parent tree number) to 0,1,2.... *)
  let num = ref 0 in
  let rs = Hashtbl.create 256 in
  iter2
    (fun pt pdfn ->
      iter (fun (k, _) -> Hashtbl.add rs (pdfn, int_of_string k) !num; num += 1) pt)
    parent_trees
    (ilist 1 (length pdfs));
  (* Process all /StructParent(s) dictionary entries to point to new /ParentTree entries. *)
  let replace_any_structparent n = function
    | ("/StructParent" | "/StructParents") as k, Pdf.Integer i ->
        begin match Hashtbl.find_opt rs (n, i) with
        | Some i' -> (k, Pdf.Integer i')
        | None -> (k, Pdf.Integer i)
        end
    | x -> x
  in
  let rec f n = function
  | Pdf.Dictionary d ->
      Pdf.recurse_dict (f n) (map (replace_any_structparent n) d)
  | Pdf.Array a ->
      Pdf.recurse_array (f n) a
  | Pdf.Stream {contents = (Pdf.Dictionary d, s)} ->
      Pdf.Stream {contents = (Pdf.recurse_dict (f n) (map (replace_any_structparent n) d), s)}
  | x -> x
  in
  iter2
    (fun pdf n ->
      Pdf.objselfmap (f n) pdf)
    pdfs (ilist 1 (length pdfs));
  (* Write the new parent tree to each file *)
  let renumbered_parent_trees =
    map2
      (fun pt pdfnum ->
         map
           (fun (k, v) -> match Hashtbl.find_opt rs (pdfnum, int_of_string k) with Some k' -> (string_of_int k', v) | None -> assert false)
           pt)
      parent_trees
      (ilist 1 (length pdfs))
  in
    (* iter2 (fun pt n -> Printf.printf "****************** FINAL PARENT TREE %i:\n" n; print_parent_tree pt) renumbered_parent_trees (ilist 1 (length pdfs)); *)
    iter2
      (fun pdf renumbered ->
         match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/StructTreeRoot"; "/ParentTree"] with
         | None -> ()
         | Some t -> Pdf.replace_chain pdf ["/Root"; "/StructTreeRoot"] ("/ParentTree", Pdftree.build_name_tree true pdf renumbered))
      pdfs
      renumbered_parent_trees
    (*;
    (* Write the PDFs to file to check them *)
    iter2
      (fun n pdf -> Pdfwrite.pdf_to_file pdf (string_of_int n ^ ".pdf"))
      (ilist 1 (length pdfs))
      pdfs*)

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
        (setify
          (option_map
            (function
             | Pdf.Dictionary d -> Some d
             | _ -> Pdfe.log "merge_dicts: not a dict"; None) dicts)))
  in
  let merge_arrays arrays =
    Pdf.Array
      (flatten
        (setify
          (option_map
            (function
             | Pdf.Array a -> Some a
             | _ -> Pdfe.log "merge_array: not an array"; None) arrays)))
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
        Pdftree.merge_name_trees_no_clash pdf (get_all struct_tree_roots pdf "/IDTree")
      in
      let merged_parenttree =
        Pdftree.merge_number_trees_no_clash pdf (get_all struct_tree_roots pdf "/ParentTree")
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
        (* 1. Get indirect references to each existing structure tree root object. They should be indirect, because /Ps will
         need to point up to them, but may not be - so if not indirect, keep direct. *)
        let existing_ks_of_struct_tree_roots_as_mostly_indirects =
          flatten
            (map
               (fun root ->
                  match Pdf.lookup_immediate "/K" root with
                  | Some (Pdf.Indirect i) -> [Pdf.Indirect i]
                  | Some (Pdf.Array a) -> a
                  | Some x -> [x]
                  | None -> [])
               struct_tree_roots)
        in
        (* 2. Rewrite in-place each previous indirect struct tree root /K member to have a /P pointing up to the new struct tree root. *)
        mkarray
          (Pdf.Array
            (map
              (function
               | Pdf.Indirect i ->
                   let d = Pdf.lookup_obj pdf i in
                     Pdf.addobj_given_num pdf (i, Pdf.add_dict_entry d "/P" (Pdf.Indirect struct_tree_objnum));
                     Pdf.Indirect i
               | Pdf.Dictionary _ as d ->
                   Pdf.add_dict_entry d "/P" (Pdf.Indirect struct_tree_objnum)
               | x -> x)
              existing_ks_of_struct_tree_roots_as_mostly_indirects))
      in
        let optional n = function
        | Pdf.Dictionary [] -> []
        | Pdf.Array [] -> []
        | x -> [(n, Pdf.Indirect (Pdf.addobj pdf x))]
        in
        let new_dict =
          Pdf.Dictionary
            (["/Type", Pdf.Name "/StructTreeRoot";
              "/ThisIs", Pdf.Name "/TheNewOne"]
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
