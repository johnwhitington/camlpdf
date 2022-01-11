(* Read and Write Destinations *)
open Pdfutil

type targetpage =
  | PageObject of int
  | OtherDocPageNumber of int

type t =
  | Action of Pdf.pdfobject
  | NullDestination
  | NamedDestinationElsewhere of string
  | XYZ of targetpage * float option * float option * float option
  | Fit of targetpage
  | FitH of targetpage * float option
  | FitV of targetpage * float option
  | FitR of targetpage * float * float * float * float
  | FitB of targetpage
  | FitBH of targetpage * float option
  | FitBV of targetpage * float option

(* Read the destination - it's either direct, or in /Dests in the document
catalog, or in /Dests in the document name tree. *)
let read_targetpage = function
  | Pdf.Indirect i -> PageObject i
  | Pdf.Integer i -> OtherDocPageNumber i
  | _ -> assert false (* ruled out in read_destination *)

(* We don't allow indirect references anywhere except in the page reference. *)
let read_destination_error n s =
  Printf.eprintf "Warning: Could not read destination %s %s \n%!" n s;
  NullDestination

let rec read_destination pdf pdfobject =
  let option_getnum = function
  | Pdf.Null -> None
  | x -> Some (Pdf.getnum x)
  in
    match Pdf.direct pdf pdfobject with
    | Pdf.Dictionary d ->
        begin
          (* We're discarding any other dictionary entries here... *)
          match Pdf.lookup_direct pdf "/D" (Pdf.Dictionary d) with
          | Some dest -> read_destination pdf dest
          | None -> NullDestination
        end
    | Pdf.Array
        [(Pdf.Indirect _ | Pdf.Integer _) as p;
         Pdf.Name "/XYZ"; l; t; z] ->
        XYZ
          (read_targetpage p, option_getnum l, option_getnum t, option_getnum z) 
    | Pdf.Array (* Read common malformed one. *)
        [(Pdf.Indirect _ | Pdf.Integer _) as p;
         Pdf.Name "/XYZ"; l; t] ->
        XYZ
          (read_targetpage p, option_getnum l, option_getnum t, None) 
    | Pdf.Array [(Pdf.Indirect _ | Pdf.Integer _) as p; Pdf.Name "/Fit"] ->
        Fit (read_targetpage p)
    | Pdf.Array [(Pdf.Indirect _ | Pdf.Integer _) as p; Pdf.Name "/FitH"; t] ->
        FitH (read_targetpage p, option_getnum t)
    | Pdf.Array [(Pdf.Indirect _ | Pdf.Integer _) as p; Pdf.Name "/FitV"; l] ->
        FitV (read_targetpage p, option_getnum l)
    | Pdf.Array [(Pdf.Indirect _ | Pdf.Integer _) as p;
                  Pdf.Name "/FitR"; l; b; r; t] ->
        FitR
          (read_targetpage p, Pdf.getnum l, Pdf.getnum b,
           Pdf.getnum r, Pdf.getnum t)
    | Pdf.Array [(Pdf.Indirect _ | Pdf.Integer _) as p; Pdf.Name "/FitB"] ->
        FitB (read_targetpage p)
    | Pdf.Array [(Pdf.Indirect _ | Pdf.Integer _) as p; Pdf.Name "/FitBH"; t] ->
        FitBH (read_targetpage p, option_getnum t)
    | Pdf.Array [(Pdf.Indirect _ | Pdf.Integer _) as p; Pdf.Name "/FitBV"; l] ->
        FitBV (read_targetpage p, option_getnum l)
    | Pdf.Name n ->
      (* PDF 1.1. Name object *)
      begin match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
      | Some catalog ->
          begin match Pdf.lookup_direct pdf "/Dests" catalog with
          | Some dests ->
              begin match Pdf.lookup_direct pdf n dests with
              | Some dest' -> read_destination pdf dest'
              | None -> NamedDestinationElsewhere n
              end
          | None -> NamedDestinationElsewhere n
          end
      | None -> raise (Pdf.PDFError "read_destination: no catalog")
      end
    | Pdf.String s ->
      (* PDF 1.2. String object *)
      let rootdict = Pdf.lookup_obj pdf pdf.Pdf.root in
        begin match Pdf.lookup_direct pdf "/Names" rootdict with
        | Some namedict ->
            begin match Pdf.lookup_direct pdf "/Dests" namedict with
            | Some destsdict ->
                begin match
                  Pdf.nametree_lookup pdf (Pdf.String s) destsdict
                with
                | None -> NamedDestinationElsewhere s
                | Some dest -> read_destination pdf (Pdf.direct pdf dest)
                end
            | _ -> NamedDestinationElsewhere s
            end
        | _ -> NamedDestinationElsewhere s 
        end
    | p -> read_destination_error "G" (Pdfwrite.string_of_pdf p)

let pdf_of_targetpage = function
  | PageObject i -> Pdf.Indirect i
  | OtherDocPageNumber i -> Pdf.Integer i

let pos_null = function
  None -> Pdf.Null
| Some x -> Pdf.Real x

let pdfobject_of_destination = function
  | Action a -> a
  | NullDestination -> Pdf.Null
  | NamedDestinationElsewhere s -> Pdf.String s
  | XYZ (p, left, top, zoom) ->
      Pdf.Array [pdf_of_targetpage p; Pdf.Name "/XYZ"; pos_null left; pos_null top; pos_null zoom]
  | Fit p ->
      Pdf.Array [pdf_of_targetpage p; Pdf.Name "/Fit"]
  | FitH (p, top) ->
      Pdf.Array [pdf_of_targetpage p; Pdf.Name "/FitH"; pos_null top]
  | FitV (p, left) ->
      Pdf.Array [pdf_of_targetpage p; Pdf.Name "/FitV"; pos_null left]
  | FitR (p, left, bottom, right, top) ->
      Pdf.Array
        [pdf_of_targetpage p; Pdf.Name "/FitR"; Pdf.Real left;
        Pdf.Real bottom; Pdf.Real right; Pdf.Real top]
  | FitB p ->
      Pdf.Array [pdf_of_targetpage p; Pdf.Name "/FitB"]
  | FitBH (p, top) ->
      Pdf.Array [pdf_of_targetpage p; Pdf.Name "/FitBH"; pos_null top]
  | FitBV (p, left) ->
      Pdf.Array [pdf_of_targetpage p; Pdf.Name "/FitBV"; pos_null left]

(* Transform destinations by a given matrix. Where we have a proper pair making
   a point, it is easy. Where we do not, we improvise. It is likely only to be
   sensible for scaling / shifting / uprighting anyway. For example, a vertical
   flip of a page is hardly likely to make a paragraph come up in the right
   position. Careful to preserve nulls, and handle all combinations. *)

(* Acrobat doesn't like /OpenActions (at least) which contain numbers outside
   the float range -32768...32768. This is the old (pre-ISO) float range. *)
let clip_pair (a, b) =
  let clip f =
    if f < -32768.0 then -32768.0 else
    if f > 32768.0 then 32768.0 else f
  in
    (clip a, clip b) 

let transform_destination t = function
  | FitH (PageObject _ as p, Some top) ->
      let (_, top) = clip_pair (Pdftransform.transform_matrix t (0., top)) in
        FitH (p, Some top)
  | FitV (PageObject _ as p, Some left) ->
      let (left, _) = clip_pair (Pdftransform.transform_matrix t (left, 0.)) in
        FitV (p, Some left)
  | FitBH (PageObject _ as p, Some top) ->
      let (_, top) = clip_pair (Pdftransform.transform_matrix t (0., top)) in
        FitBH (p, Some top)
  | FitBV (PageObject _ as p, Some left) ->
      let (left, _) = clip_pair (Pdftransform.transform_matrix t (left, 0.)) in
        FitBV (p, Some left)
  | XYZ (PageObject _ as p, Some left, Some top, zoom) ->
      let left, top = clip_pair (Pdftransform.transform_matrix t (left, top)) in
        XYZ (p, Some left, Some top, zoom)
  | XYZ (PageObject _ as p, None, Some top, zoom) ->
      let _, top = clip_pair (Pdftransform.transform_matrix t (0., top)) in
        XYZ (p, None, Some top, zoom)
  | XYZ (PageObject _ as p, Some left, None, zoom) ->
      let left, _ = clip_pair (Pdftransform.transform_matrix t (left, 0.)) in
        XYZ (p, Some left, None, zoom)
  | FitR (PageObject _ as p, left, bottom, right, top) ->
      let left, top = clip_pair (Pdftransform.transform_matrix t (left, top)) in
      let right, bottom = clip_pair (Pdftransform.transform_matrix t (right, bottom)) in
        FitR (p, left, bottom, right, top)
  | x -> x
