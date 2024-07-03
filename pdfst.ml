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
      iter (Pdf.removeobj pdf) (setify_large !del);
      (* Any /K referencing these deleted objects is modifed to no longer reference
         it. We don't rely on nulls, but do the actual modification. *)
      Pdf.objiter
        (fun n o ->
           ())
        pdf;
      flprint "trim_structure_tree\n"
