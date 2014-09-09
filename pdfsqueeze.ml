open Pdfutil

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

let squeeze pdf =
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

