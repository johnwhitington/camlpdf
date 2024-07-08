(* Operations on structure trees. *)
open Pdfutil

(* Recursion between modules *)
let endpage = ref (fun _ -> 0)

(* Rewrite the parent tree to remove references to objects which no longer
   exist (deleted pages), or which are referenced only from it (e.g annots and
   xobjects of now-deleted pages. Pdfpage.pdf_of_pages calls this when it is
   nearly finished. *)
let postprocess_parent_tree pdf =
  match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/StructTreeRoot"; "/ParentTree"] with
  | None -> ()
  | Some t ->
      let pt = Pdftree.read_number_tree pdf t in
      let pt = lose (function (_, Pdf.Null) -> true | _ -> false) pt in
        Pdf.replace_chain pdf ["/Root"; "/StructTreeRoot"] "/ParentTree" (Pdftree.build_name_tree true pdf pt)

(* FIXME Do we need to also process the /ParentTree to remove references to
   annots etc. which would othersie be deleted? This is not a correctness issue,
   but is a size one - objects will end up in the output file only because they
   are linked from the /ParentTree. *)

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
      while !del <> [] do
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
             (* Takes a list of integers or indirects. Indirects could be
                structelem or marked-content reference dictionary or object
                reference dictionary depending on /Type (no /Type = assume
                structelem). Returns the None if no change, or Some list with
                all which point to hitherto deleted objects removed. *)
             let process objs = 
               let survives = function
                 | Pdf.Indirect i ->
                     (* Must a) still exist in this file *)
                     Pdf.lookup_obj pdf i <> Pdf.Null  &&
                     (* b) not be an object reference dictionary referencing a /Pg which is to be deleted *)
                     (* c) not be an object reference dictionary with a /Obj which is a deleted page *)
                     begin match Pdf.indirect_number pdf "/Pg" (Pdf.Indirect i), Pdf.indirect_number pdf "/Obj" (Pdf.Indirect i) with
                     | Some i, _ when mem i page_objnums_to_remove -> false
                     | _, Some i when mem i page_objnums_to_remove -> false
                     | _ -> true
                     end (* FIXME this was speculative. Check. *)
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
                   | _ -> ()
                   end
             | _ -> ())
          pdf
        done

(* FIXME Are we sure no other objects can have /K or /Pg? Do we need to read
   the struct tree object list to restrict what we touch? *)

(* FIXME Eventually, will we need to repalace a proper model which parses
   the whole structure tree, modifies it as a tree and re-writes it? *)

(* FIXME Do we need to look inside content streams to find MCIDs and make sure
   no MCID remaining in the tree points to a now non-extant one? Does the
   verifier care? *)

(* FIXME Do we need to rewrite /ParentTree to remove nulls after pdf_of_pages
   is finished? For efficiency yes, but for correctness too? *)
