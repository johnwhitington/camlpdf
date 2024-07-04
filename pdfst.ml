(* Operations on structure trees. *)
open Pdfutil

let endpage = ref (fun _ -> 0)

(* Remove any structure tree node (and therefore its children) which has a page
   number pointing to a page not to be included in the output. This should be a
   reasonable first approximation to the required behaviour. *)
let trim_structure_tree pdf range =
  let page_objnums_to_remove =
    let objnums = Pdf.page_reference_numbers pdf in
      map (List.nth objnums) (setminus (ilist 1 (!endpage pdf)) range)
  in
    (* Any object with /Pg not in range is deleted. *)
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
      (* Any /K referencing these deleted objects is modifed to no longer reference
         it. We don't rely on nulls, but do the actual modification. *)
      while !del <> [] do
        iter (Pdf.removeobj pdf) (setify_large !del);
        del := [];
        Pdf.objiter
          (fun n o ->
             let process objs = None in
             match Pdf.direct pdf o with
             | Pdf.Dictionary d ->
                 begin match List.assoc_opt "/K" d with
                 | Some (Pdf.Indirect _ | Pdf.Integer _ as obj) ->
                     (* Could be structelem or marked-content reference dictionary
                     or object reference dictionary depending on /Type (no /Type =
                     assume structelem *)
                     begin match process [obj] with
                     | None -> () (* no change *)
                     | Some newlist ->
                         (* update the value of /K and change the object in place *)
                         ()
                     end
                 | Some (Pdf.Array objs) ->
                     (* An array of indirects or integers. Empty arrays not allowed. *)
                     begin match process objs with
                     | None -> () (* no change *)
                     | Some [] ->
                         (* empty not allowed - we must now delete this object?
                            But what if it chains up? Go until no more change? *)
                         ()
                     | Some newlist ->
                         (* update the value of /K and change the object in place *)
                         ()
                     end
                 | _ -> ()
                 end
             | _ -> ())
          pdf
        done


(* FIXME Are we sure no other objects can have /K or /Pg? Do we need to read
   the struct tree object list to restrict what we touch? *)

(* FIXME Do we need a more careful approach anyway? What does the verifier /
   spec have to say on the matter? *)

(* FIXME Is our approach tough enough - should we be cutting structures further
   up the tree if they don't correspond to anything marked with a /Pg? *)

(* FIXME Eventually, this will be replaced with a proper model which a) parses
   the whole structure tree, modifies it as a tree and re-writes it *)

(* FIXME Do we need to look inside content streams to find MCIDs and make sure
   no MCID remaining in the tree points to a now non-extant one? Does the
   verifier care? *)
