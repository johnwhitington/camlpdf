(* Operations on structure trees. *)
open Pdfutil

(* Recursion between modules *)
let endpage = ref (fun _ -> 0)

(* Rewrite the parent tree to remove references to objects which no longer
   exist (deleted pages), or which are referenced only from it (e.g annots and
   xobjects of now-deleted pages. Pdfpage.pdf_of_pages calls this when it is
   nearly finished. *)
let remove_nulled_element pdf ((k : string), (v : Pdf.pdfobject)) =
  match v with
  | Pdf.Indirect i when Pdf.lookup_obj pdf i = Pdf.Null -> (k, Pdf.Null)
  | Pdf.Array elts ->
      begin match keep (function Pdf.Indirect i when Pdf.lookup_obj pdf i = Pdf.Null -> false | _ -> true) elts with
      | [] -> (k, Pdf.Null)
      | elts -> (k, Pdf.Array elts)
      end
  | _ -> (k, v)

(* FIXME Do we need to also process the /ParentTree to remove references to
   annots etc. which would otherwise be deleted? Does pdf_of_pages remove
   those? This is not a correctness issue, but is a size one - objects will end
   up in the output file only because they are linked from the /ParentTree. *)
let postprocess_parent_tree pdf =
  match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/StructTreeRoot"; "/ParentTree"] with
  | None -> ()
  | Some t ->
      let pt = Pdftree.read_number_tree pdf t in
      let pt = map (remove_nulled_element pdf) pt in
        (*iter (fun (n, v) -> Printf.printf "%s -> %s\n" n (Pdfwrite.string_of_pdf v)) pt;*)
        Pdf.replace_chain pdf ["/Root"; "/StructTreeRoot"] ("/ParentTree", Pdftree.build_name_tree true pdf pt)

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

(* Merge structure hierarchy / tagged PDF.

/IDTree                 name tree      *rename and merge
/ParentTree             number tree    *renumber and merge
/ParentTreeNextKey      integer        *update
/RoleMap                dict           *rename to be consistent if possible, if not degrade, and merge
/ClassMap               dict           *rename and merge
/NameSpaces             array          *rename and merge
/PronunciationLexicon   array          concatenate
/AF                     array          concatenate
/K                      structure tree merge trees *)

(* Preprocessing step. Renumber parent trees, and MCIDs pointing to them, not to clash. *)

let renumber_mcids pdf rs = function
  | Pdfops.Op_BDC (s, d) as op ->
      begin match Pdf.lookup_chain pdf d ["/P"; "/MCID"] with
      | Some (Pdf.Integer i) ->
          begin match Hashtbl.find_opt rs i with
          | Some i' -> Pdfops.Op_BDC (s, Pdf.replace_chain_all_direct d ["/P"] ("/MCID", Pdf.Integer i'))
          | None -> op
          end
      | _ -> op
      end
  | x -> x

let renumber_parent_trees pdfs =
  (* Get the parent trees *)
  let parent_trees =
    map
      (fun pdf ->
         match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/StructTreeRoot"; "/ParentTree"] with
         | Some t -> Pdftree.read_number_tree pdf t
         | None -> [])
    pdfs
  in
  (* Calculate a renumbering mapping from 0 upwards *)
  let rs =
    Hashtbl.create 256
  in
  (* Process all the content streams and xobjects in each file to apply the numbering. *)
  iter
    (fun pdf -> Pdf.objiter
      (fun n o ->
         match Pdf.lookup_direct pdf "/Type" o, Pdf.lookup_direct pdf "/Subtype" o with
         | Some (Pdf.Name "/Page"), _ ->
             (* We are allowed to smash /Contents in modern PDF, which a file with a /Parent should be. *)
             ()
         | _, Some (Pdf.Name "/Form") ->
             (* Just do this stream. *)
             ()
         | x -> ())
      pdf)
    pdfs;
  (* Write the new parent tree to each file *)
  let renumbered_parent_trees =
    map_lol (fun (k, v) -> match Hashtbl.find_opt rs k with Some k' -> (k', v) | None -> assert false) parent_trees
  in
    iter2
      (fun pdf renumbered ->
         match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/StructTreeRoot"; "/ParentTree"] with
         | None -> ()
         | Some t -> Pdf.replace_chain pdf ["/Root"; "/StructTreeRoot"] ("/ParentTree", Pdftree.build_name_tree true pdf renumbered))
      pdfs
      renumbered_parent_trees

let merge_structure_hierarchy pdf pdfs =
  renumber_parent_trees pdfs;
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
