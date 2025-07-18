open Pdfutil

(* The type of the four rotations of pages. This defines how a viewing
application (e.g Acrobat) displays the page. *)
type rotation =
  | Rotate0
  | Rotate90
  | Rotate180
  | Rotate270

(* A type representing a page. content is the list of objects
containing the graphical content stream (see the Pdfops module), mediabox
the page size, resources the page's resource dictionary, rotate its rotation
and rest any other entries to reside in the page dictionary. *)
type t =
  {content : Pdf.pdfobject list;
   mediabox : Pdf.pdfobject;
   resources : Pdf.pdfobject;
   rotate : rotation;
   rest : Pdf.pdfobject} (* A dictionary of the other entries in the page dictionary. *)

(* Make a PDF rectangle from a Paper.papersize. *)
let rectangle_of_paper paper =
  let u = Pdfpaper.unit paper in
  let w = Pdfunits.points (Pdfpaper.width paper) u in
  let h = Pdfunits.points (Pdfpaper.height paper) u in
    Pdf.Array [Pdf.Real 0.; Pdf.Real 0.; Pdf.Real w; Pdf.Real h]

(* Create a page with empty content, media box from the given paper size,
empty resources, zero rotation and no extra dictionary entries. *)
let custompage rectangle =
  {content = [];
   mediabox = rectangle;
   resources = Pdf.Dictionary [];
   rotate = Rotate0;
   rest = Pdf.Dictionary []}

let blankpage papersize =
  custompage (rectangle_of_paper papersize)

(* Utility function to convert from rotation to integers. *)
let int_of_rotation = function
  | Rotate0 -> 0
  | Rotate90 -> 90
  | Rotate180 -> 180
  | Rotate270 -> 270

(* The reverse. raises [Pdf.PDFError] if its input modulo 360 is not 0, 90, 
180, 270, -90, -180 or -270. *)
let rotation_of_int i =
  match i mod 360 with
  | 0 -> Rotate0
  | 90 | -270 -> Rotate90
  | 180 | -180 -> Rotate180
  | 270 | -90 -> Rotate270
  | _ -> raise (Pdf.PDFError "Bad /Rotate")

(* Extracting the page tree *)

let rec remove_dict_entries e = function
  | (("/Resources" | "/Contents" | "/MediaBox" | "/Rotate" | "/Parent" | "/Type"), v)::t -> remove_dict_entries e t
  | h::t -> remove_dict_entries (h::e) t
  | [] -> e

(* Given a page tree, find the first page resources, contents and
mediabox.  The resources and mediabox may be inherited from any node above in
the page tree. *)

(* Some files erroneously miss out a mediabox, expecting the reader to inherit
it not from the page tree node above, but from the previous page. Most PDF
readers can do this, and GhostScript can too. So we adopt this behaviour in the
case of a missing mediabox. These readers also use US Letter Portrait as the
default in case of no mediabox being present at all. *)
let last_mediabox_seen =
  ref (Pdf.Array [Pdf.Integer 0; Pdf.Integer 0; Pdf.Integer 612; Pdf.Integer 792])

let rec find_pages pages pdf resources mediabox rotate =
  match pages with Pdf.Null -> [] | _ ->
  match Pdf.lookup_direct pdf "/Type" pages with
  | Some (Pdf.Name "/Pages") | None ->
      begin match
        Pdf.lookup_fail "No /Kids in page tree" pdf "/Kids" pages
      with
      | Pdf.Array kids ->
          let kids =
            map
              (function
               | Pdf.Indirect k ->
                   (try Pdf.lookup_obj pdf k with
                     Not_found -> raise (Pdf.PDFError "missing kid\n"))
               | Pdf.Dictionary d -> Pdf.Dictionary d (* malformed but we allow *)
               | _ -> raise (Pdf.PDFError "malformed kid\n"))
              kids
          in
            let resources =
              match Pdf.lookup_direct pdf "/Resources" pages with
              | Some x -> Some x
              | None -> resources
            in let mediabox =
              match Pdf.lookup_direct pdf "/MediaBox" pages with
              | Some x -> Some x
              | None -> mediabox
            in let rotate =
              match Pdf.lookup_direct pdf "/Rotate" pages with
              | Some (Pdf.Integer r) -> rotation_of_int r
              | _ -> rotate
            in
              flatten
                (map
                  (fun k -> find_pages k pdf resources mediabox rotate)
                  kids)
      | _ -> raise (Pdf.PDFError "Malformed /Kids in page tree node")
      end
  | Some _ ->
      let resources =
        match Pdf.lookup_direct pdf "/Resources" pages with
        | Some x -> Some x
        | None -> resources
      in let mediabox =
        match Pdf.lookup_direct pdf "/MediaBox" pages with
        | Some x -> Some x
        | None -> mediabox
      in let contents =
        (* 28th March 2016. We modify this to always create an array of one
        indirect if just a single contents stream, rather than following
        through to the actual object. Other code can then preserve the
        sharing. *)
        begin match pages with
          Pdf.Dictionary d ->
            begin match lookup "/Contents" d with
              Some (Pdf.Indirect i) ->
                (* A single content stream, or indirect to array *)
                begin match Pdf.lookup_obj pdf i with
                | Pdf.Array a -> Some (Pdf.Array a)
                | _ -> Some (Pdf.Array [Pdf.Indirect i])
                end
            | _ ->
                (* An array of indirects. Just return it *)
                Pdf.lookup_direct pdf "/Contents" pages
            end
        | _ -> raise (Pdf.PDFError "Pages not a dictionary")
        end
      in let rotate =
        match Pdf.lookup_direct pdf "/Rotate" pages with
        | Some (Pdf.Integer r) -> rotation_of_int r
        | _ -> rotate
      in
        [{resources =
            (match resources with
            | Some r -> r
            | None -> Pdf.Dictionary []);
          content =
            (* 15th Feb 2012. We now preserve indirect references in /Contents
            to preserve sharing, at least on operations which don't modify the
            /Contents. *)
            (match contents with
            | None -> []
            | Some (Pdf.Array cs) ->
                option_map
                  (function x ->
                     match Pdf.direct pdf x with
                     | Pdf.Stream _ -> Some x
                     | x -> Pdfe.log "Page /Contents not a stream: ignoring.\n"; None)
                  cs;
            | Some pdfobject ->
                begin match Pdf.direct pdf pdfobject with
                | Pdf.Stream _ -> [pdfobject]
                | _ -> Pdfe.log "Page /Contents not a stream: ignoring.\n"; []
                end);
          mediabox =
            (match mediabox with
            | Some m -> last_mediabox_seen := m; m
            | None ->
                Pdfe.log "Warning: missing mediabox. Using most recently seen.\n";
                !last_mediabox_seen);
          rotate = rotate;
          rest =
            (match pages with
            | Pdf.Dictionary d -> Pdf.Dictionary (remove_dict_entries [] d)
            | _ -> raise (Pdf.PDFError "Bad /Pages"))
        }]

(* Given a pdf, return a list of (resources, contents, mediabox) triples. *)
let pages_of_pagetree pdf =
  let document_catalog =
    try Pdf.lookup_obj pdf pdf.Pdf.root with
      Not_found -> raise (Pdf.PDFError "/Root entry is incorrect")
  in 
    let pages =
      Pdf.lookup_fail "No or malformed /Pages" pdf "/Pages" document_catalog
    in
      find_pages pages pdf None None Rotate0

let rec find_pages_quick count pages pdf =
  match count with 10000 -> raise (Pdf.PDFError "cycle in page tree") | _ -> 
  match pages with Pdf.Null -> 0 | _ ->
  match Pdf.lookup_direct pdf "/Type" pages with
  | Some (Pdf.Name "/Pages") | None ->
      begin match
        Pdf.lookup_fail "No /Kids in page tree" pdf "/Kids" pages
      with
      | Pdf.Array kids ->
          let kids =
            map
              (function
               | Pdf.Indirect k ->
                   (try Pdf.lookup_obj pdf k with
                     Not_found -> raise (Pdf.PDFError "missing kid\n"))
               | Pdf.Dictionary d -> Pdf.Dictionary d (* malformed, but we allow *)
               | x -> raise (Pdf.PDFError "malformed kid\n"))
              kids
          in
            sum (map (fun k -> find_pages_quick (count + 1) k pdf) kids)
      | _ -> raise (Pdf.PDFError "Malformed /Kids in page tree node")
      end
  | Some _ -> 1

let pages_of_pagetree_quick pdf =
  let document_catalog =
    try Pdf.lookup_obj pdf pdf.Pdf.root with
      Not_found -> raise (Pdf.PDFError "/Root entry is incorrect")
  in 
    let pages =
      Pdf.lookup_fail "No or malformed /Pages" pdf "/Pages" document_catalog
    in
      find_pages_quick 0 pages pdf

let endpage pdf =
  pages_of_pagetree_quick pdf

let endpage_fast pdf =
  let document_catalog =
    try Pdf.lookup_obj pdf pdf.Pdf.root with
      Not_found -> raise (Pdf.PDFError "/Root entry is incorrect")
  in 
  let pages =
    Pdf.lookup_fail "No or malformed /Pages" pdf "/Pages" document_catalog
  in
    match Pdf.lookup_direct pdf "/Count" pages with
    | Some (Pdf.Integer i) -> i
    | _ -> 0

let _ =
  Pdfread.endpage := endpage;
  Pdfst.endpage := endpage

(* Make a collection of pages capable of being merged -- in other words rename
their resources so as not to clash. *)
let source k =
  let k = ref k in (fun () -> incr k; !k)

let freshname source =
  "/r" ^ string_of_int (source ())

let resource_keys =
  ["/Font"; "/ExtGState"; "/ColorSpace";
   "/Pattern"; "/Shading"; "/XObject"; "/Properties"]

let make_changes pdf pages =
  let src = source 0 in
    let entries_of_page entry pageseq page =
      let entries =
        match Pdf.lookup_direct pdf entry page.resources with
        | Some (Pdf.Dictionary es) -> es
        | _ -> []
      in
        map (fun (k, v) -> entry, pageseq, k, freshname src) entries
    in
      let pagenums = ilist 1 (length pages) in
        let entries name =
          map2 (entries_of_page name) pagenums pages
        in
          let entries = flatten <| flatten (map entries resource_keys) in
            let table = Hashtbl.create 1000 in
              iter
                (fun (entry, pageseq, k, name) ->
                   Hashtbl.add table (entry, pageseq, k) name)
                entries;
              table

let change_operator pdf lookup lookup_option seqnum = function
  | Pdfops.Op_Tf (f, s) ->
      Pdfops.Op_Tf (lookup "/Font" seqnum f, s)
  | Pdfops.Op_gs n ->
      Pdfops.Op_gs (lookup "/ExtGState" seqnum n)
  | Pdfops.Op_CS n ->
      begin match lookup_option "/ColorSpace" seqnum n with
      | Some x -> Pdfops.Op_CS x
      | None -> Pdfops.Op_CS n
      end
  | Pdfops.Op_cs n ->
      begin match lookup_option "/ColorSpace" seqnum n with
      | Some x -> Pdfops.Op_cs x
      | None -> Pdfops.Op_cs n
      end
  | Pdfops.Op_SCNName (s, ns) ->
      Pdfops.Op_SCNName (lookup "/Pattern" seqnum s, ns)
  | Pdfops.Op_scnName (s, ns) ->
      Pdfops.Op_scnName (lookup "/Pattern" seqnum s, ns)
  | Pdfops.Op_sh s ->
      Pdfops.Op_sh (lookup "/Shading" seqnum s)
  | Pdfops.Op_Do x ->
      Pdfops.Op_Do (lookup "/XObject" seqnum x)
  | Pdfops.Op_DP (n, Pdf.Name p) ->
      Pdfops.Op_DP (n, Pdf.Name (lookup "/Properties" seqnum p))
  | Pdfops.Op_BDC (n, Pdf.Name p) ->
      begin match lookup_option "/Properties" seqnum p with
        | Some x ->
            Pdfops.Op_BDC (n, Pdf.Name x)
        | None ->
            Pdfe.log "Warning: Missing Op_BDC /Properties entry\n";
            Pdfops.Op_BDC (n, Pdf.Name p)
      end
  | Pdfops.InlineImage (dict, dp, bytes) ->
      (* Replace any indirect "/CS" or "/ColorSpace" with a new "/CS" *)
      let dict' =
        match Pdf.lookup_direct_orelse pdf "/CS" "/ColorSpace" dict with
        | Some (Pdf.Name "/DeviceGray")
        | Some (Pdf.Name "/DeviceRGB")
        | Some (Pdf.Name "/DeviceCMYK")
        | Some (Pdf.Name "/G")
        | Some (Pdf.Name "/RGB")
        | Some (Pdf.Name "/CMYK") -> dict
        | Some (Pdf.Name n) ->
            Pdf.add_dict_entry
              (Pdf.remove_dict_entry
                (Pdf.remove_dict_entry dict "/ColorSpace")
                "/CS")
              "/CS"
              (Pdf.Name (lookup "/ColorSpace" seqnum n))
        | _ -> dict
      in
        Pdfops.InlineImage (dict', dp, bytes)
  | x -> x

(* Only for use with twoup now. FIXME: Can blow up shared content streams. Needs
a cunning new method to preserve sharing. *)
let renumber_pages pdf pages =
  match pages with
  | [] -> []
  | pages ->
      let changes = make_changes pdf pages in
        let lookup_option dictname page oldkey =
          tryfind changes (dictname, page, oldkey)
        and lookup dictname page oldkey =
          try
            Hashtbl.find changes (dictname, page, oldkey)
          with
            Not_found -> raise (Pdf.PDFError "Pdfdoc.renumber_pages: Bad key")
        in
        let change_content seqnum resources content =
          let operators = Pdfops.parse_operators pdf resources content in
            let operators' =
              map (change_operator pdf lookup lookup_option seqnum) operators
            in
              [Pdfops.stream_of_ops operators']
        in let change_resources seqnum resources =
          let newdict name =
            match Pdf.lookup_direct pdf name resources with
            | Some (Pdf.Dictionary fonts) ->
                Pdf.Dictionary (map (fun (k, v) -> lookup name seqnum k, v) fonts)
            | _ -> Pdf.Dictionary []
          in
            let newdicts = map newdict resource_keys in
              let resources = ref resources in
                iter2
                  (fun k v ->
                    resources := Pdf.add_dict_entry !resources k v)
                  resource_keys
                  newdicts;
                !resources
        in
          let process_page seqnum page =
            {page with
               content = change_content seqnum page.resources page.content;
               resources = change_resources seqnum page.resources}
          in
            map2 process_page (indx pages) pages

(* New code for better page trees *)

(* Each branch contains a list of pages to go at that branch, and pointers to
two more page tree nodes.  Each leaf contains just a page list. Page lists must
be non-null.

Leaves and branches also hold a parent pointer, and the object number of that
leaf or branch. *) 
type ptree =
  | Lf of t list * int * int
  | Br of t list * ptree * ptree * int * int

(* Split a list into three parts, the middle being of fixed, given, length n,
and the left and right roughly equal in size, but at least of length one. *)
let split3 n l =
  let len = length l in
    if n > len - 2 then raise (Invalid_argument "split3") else
      let leftlen = (len - n) / 2 in
        let left, rest = cleave l leftlen in
          let middle, right = cleave rest n in
            left, middle, right

(* Build the pages *)
let rec pagetree objnumsource pages parent =
  if length pages < 10 then Lf (pages, parent, objnumsource ()) else
    let left, this, right = split3 5 pages in
      let this_num = objnumsource () in
        let left_tree = pagetree objnumsource left this_num
        in let right_tree = pagetree objnumsource right this_num in
          Br (this, left_tree, right_tree, parent, this_num)

(*let pagetree_flat objnumsource pages parent =
  Lf (pages, parent, objnumsource ())*)

(* Version for pdf_of_pages where we are using the same object numbers *)
type ptree_objnumbers =
  | OLf of int list * int * int (* object numbers, parent, object number of this leaf *)
  | OBr of int list * ptree_objnumbers * ptree_objnumbers * int * int (* object numbers, left, right, parent, object number of this branch *)

(*let rec print_ptree = function
  | OLf (is, parent, objnumleaf) ->
      Printf.printf "OLf with object numbers ";
      print_ints is;
      Printf.printf " parent %i and this leaf object number is %i\n" parent objnumleaf
  | OBr (is, l, r, p, thisobjnumbranch) ->
      Printf.printf "OBt with object numbers ";
      print_ints is;
      Printf.printf " parent %i, this object number is %i\n" p thisobjnumbranch;
      Printf.printf "***LEFTS\n";
      print_ptree l;
      Printf.printf "***RIGHTS\n";
      print_ptree r*)

let rec pagetree_with_objnumbers toplevel old_pagetree_root_num objnumsource objnumbers parent =
  if length objnumbers < 10 then
    OLf (objnumbers, parent, if toplevel then old_pagetree_root_num else objnumsource ())
  else
    let left, this, right = split3 5 objnumbers
    and this_num = if toplevel then old_pagetree_root_num else objnumsource () in
      let left_tree = pagetree_with_objnumbers false old_pagetree_root_num objnumsource left this_num
      and right_tree = pagetree_with_objnumbers false old_pagetree_root_num objnumsource right this_num in
        OBr (this, left_tree, right_tree, parent, this_num)

(* Make a page. Returns, objectnumber, page pdfobject, extra objects to be added. *)
let mkpage getobjnum parent page =
  (*Printf.printf "mkpage with parent %i\n" parent;*)
  let content, extras =
    match page.content with
    | [] -> [], []  (*r Null Contents not allowed. *)
    | cs ->
       let indirects, objects =
          split
            (map
              (function
                 | Pdf.Indirect i -> Pdf.Indirect i, None
                 | c -> let i = getobjnum () in Pdf.Indirect i, Some (i, c))
              cs)
        in
          [("/Contents", Pdf.Array indirects)], losenones objects 
  in
    let page =
      Pdf.Dictionary
        ([("/Type", Pdf.Name "/Page");
          ("/Parent", Pdf.Indirect parent);
          ("/Resources", page.resources);
          ("/MediaBox", page.mediabox)]
         @
          (let i = int_of_rotation page.rotate in if i = 0 then [] else [("/Rotate", Pdf.Integer i)])
      @
        (match page.rest with
         | Pdf.Dictionary d -> d
         | _ -> raise (Pdf.PDFError "mkpage"))
      @ 
        content)
    in
      getobjnum (), page, extras

(* Build a list of objnum, pdfobject pairs from the ptree. The pages in the
ptree are just missing their parent entries, so we add those. *)
let rec objects_of_ptree getobjnum extras = function
  | Lf (pages, parent, this) ->
      let page_objects =
        map
         (fun (o, p, x) -> extras =@ x; (o, p))
         (map (mkpage getobjnum this) pages)
      in
        let page_tree_node =
          let pdfobject =
            let parent_entry =
              if parent = 0 then [] else ["/Parent", Pdf.Indirect parent]
            in
              Pdf.Dictionary
                (["/Type", Pdf.Name "/Pages";
                  "/Kids",
                     Pdf.Array (
                       map (fun x -> Pdf.Indirect x) (fst <| split page_objects));
                  "/Count", Pdf.Integer (length pages)]
                 @ parent_entry)
          in
           this, pdfobject 
        in
          page_tree_node::page_objects
  | Br (pages, left, right, parent, this) ->
      let objs_left = objects_of_ptree getobjnum extras left
      in let objs_right = objects_of_ptree getobjnum extras right in
        let left_num =
          match objs_left with
          | (n, _)::_ -> n
          | [] -> assert false
        in let right_num =
          match objs_right with
          | (n, _)::_ -> n
          | [] -> assert false
        in let count_left =
          match objs_left with
          | (_, Pdf.Dictionary d)::_ ->
              begin match lookup "/Count" d with
              | Some (Pdf.Integer i) -> i 
              | _ -> assert false
              end
          | _ -> assert false
        in let count_right =
          match objs_right with
          | (_, Pdf.Dictionary d)::_ ->
              begin match lookup "/Count" d with
              | Some (Pdf.Integer i) -> i 
              | _ -> assert false
              end
          | _ -> assert false
        in
          let this_objects =
            let page_objects =
              map
               (fun (o, p, x) -> extras =@ x; (o, p))
               (map (mkpage getobjnum this) pages)
            in
              let page_tree_node =
                let pdfobject =
                  let parent_entry =
                    if parent = 0 then [] else ["/Parent", Pdf.Indirect parent]
                  in
                    let kids = fst <| split page_objects in
                      Pdf.Dictionary
                        (["/Type", Pdf.Name "/Pages";
                          "/Kids",
                             Pdf.Array
                               (map
                                  (fun x -> Pdf.Indirect x)
                                  ([left_num] @ kids @ [right_num]));
                          "/Count", Pdf.Integer (count_left + count_right + length kids)]
                         @ parent_entry)
                in
                 this, pdfobject 
              in
                page_tree_node::page_objects
           in
             this_objects @ objs_left @ objs_right
     
(* Take a list of pages and a PDF. Build a page tree in the PDF, returning
the new pdf and the object number assigned to the top page node. All references
to objects not forming part of the tree nodes themselves are left unchanged. *)
let add_pagetree pages pdf =
  let extras = ref [] in
    let getobjnum = source pdf.Pdf.objects.Pdf.maxobjnum in
      let ptree = pagetree getobjnum pages 0 in
        let objects = objects_of_ptree getobjnum extras ptree in
          let topnode = match hd objects with (n, _) -> n in
            iter (fun x -> ignore (Pdf.addobj_given_num pdf x)) (objects @ !extras);
            pdf, topnode

(* Add a root entry, replacing the Type and Pages entry, and any entries in
extras. Preserves any entries in any existing root (e.g Metadata pointer). *)
let add_root pageroot extras pdf =
  let existing_entries =
    try
      match Pdf.lookup_obj pdf pdf.Pdf.root with
      | Pdf.Dictionary d -> d
      | _ -> []
    with
    _ -> []
  in
    let root =
      Pdf.Dictionary
        (fold_right (* Right so that /Type, /Pages overwrite *)
           (fun (k, v) d -> add k v d)
              ([("/Type", Pdf.Name "/Catalog"); ("/Pages", Pdf.Indirect pageroot)] @ existing_entries)
              extras)
    in
      let rootnum = Pdf.addobj pdf root in
        let trailerdict' =
          match pdf.Pdf.trailerdict with
          | Pdf.Dictionary d -> Pdf.Dictionary (add "/Root" (Pdf.Indirect rootnum) d)
          | _ -> raise (Pdf.PDFError "add_root: bad trailer dictionary")
        in
          {pdf with
             Pdf.root = rootnum;
             Pdf.trailerdict = trailerdict'}

(* Make sure to supply refnums to speed it up, if you already have them from a
 * previous call to Pdf.page_reference_numbers *)
let rec pagenumber_of_target ?fastrefnums pdf = function
 | Pdfdest.NullDestination -> 0
 | Pdfdest.NamedDestination _ -> 0
 | Pdfdest.StringDestination _ -> 0
 | Pdfdest.Action a ->
     begin match Pdf.lookup_direct pdf "/S" a, Pdf.lookup_direct pdf "/D" a with
     | Some (Pdf.Name "/GoTo"), Some dest ->
         pagenumber_of_target ?fastrefnums pdf (Pdfdest.read_destination pdf dest)
     | _ -> 0
     end
 | Pdfdest.XYZ (t, _, _, _) | Pdfdest.Fit t | Pdfdest.FitH (t, _) | Pdfdest.FitV (t, _)
 | Pdfdest.FitR (t, _, _, _, _) | Pdfdest.FitB t | Pdfdest.FitBH (t, _) | Pdfdest.FitBV (t, _) ->
     match t with
     | Pdfdest.OtherDocPageNumber i -> i + 1 (* If it's really a Pdfdest.OtherDocPageNumber, you must process this yourself before. *)
     | Pdfdest.PageObject i ->
         match fastrefnums with
         | Some table ->
             begin try Hashtbl.find table i with Not_found -> 0 end 
         | None ->
             match position_1 i (Pdf.page_reference_numbers pdf) with
             | Some n -> n
             | None -> 0

(* Return a new PDF containing everything the old one does, but with new pages.

Other objects (e.g destinations in the document outline) may point to the
individual page objects, so we must renumber these. We can only do this if the
number of pages are the same. We do this [if change_references is true]. If the
new and old page lists are of different lengths, change_references must be
false, or you must supply the changes (expressed as (from, to) 1-based serial
number pairs).

The matrices optional argument, only relevant when change_references is true
and the number of pages has not changed, gives a list of (page number, matrix)
pairs which indicate that the page has been transformed. We can then rewrite
bookmark destinations to reflect the transformed destination positions. We also
rewrite annotation destinations after the same fashion. *)
let change_pages_find_matrix dest mattable refnumstable =
  match dest with
  | Pdfdest.XYZ (tp, _, _, _) | Pdfdest.FitH (tp, _) | Pdfdest.FitV (tp, _)
  | Pdfdest.FitR (tp, _, _, _, _) | Pdfdest.FitBH (tp, _) | Pdfdest.FitBV (tp, _) ->
      begin match tp with
        Pdfdest.PageObject i ->
          begin try
            let pagenumber = Hashtbl.find refnumstable i in
              Hashtbl.find mattable pagenumber
          with
            _ ->
              (*Pdfe.log (Printf.sprintf
                "page not found for bookmark or annotation dest:%s\n"
                (Pdfwrite.string_of_pdf (Pdfdest.pdfobject_of_destination dest)));*)
              Pdftransform.i_matrix
          end
      | _ -> Pdftransform.i_matrix
      end
  | _ -> Pdftransform.i_matrix

(* For each bookmark, find the page its target is on, look up the appropriate matrix, and transform it. Works only for destinations. /GoTo actions are rewritten globally, separately. *)
let change_pages_process_bookmarks mattable refnumstable pdf =
  (*List.iter (fun (p, m) -> Printf.printf "chppb: %i = %s\n" p (Pdftransform.string_of_matrix m)) matpairs;*)
  let bookmarks =
    map
      (fun m ->
         let tr = change_pages_find_matrix m.Pdfmarks.target mattable refnumstable in
           if tr <> Pdftransform.i_matrix then Pdfmarks.transform_bookmark pdf tr m else m)
      (Pdfmarks.read_bookmarks ~preserve_actions:true pdf)
  in
    Pdfmarks.add_bookmarks bookmarks pdf 

let rewrite_dest pdf mattable refnumstable dest =
  let parsed_dest = Pdfdest.read_destination pdf dest in
  let tr = change_pages_find_matrix parsed_dest mattable refnumstable in
    (*Printf.printf "tr is %s\n" (Pdftransform.string_of_matrix tr);*)
    if tr <> Pdftransform.i_matrix then
      let transformed = Pdfdest.transform_destination pdf tr parsed_dest in
      let new_dest = Pdfdest.pdfobject_of_destination transformed in
        Some (Pdf.addobj pdf new_dest)
    else
      None

let rewrite_action pdf mattable refnumstable action =
  begin match Pdf.lookup_direct pdf "/S" action with
  | Some (Pdf.Name "/GoTo") ->
    begin match Pdf.lookup_direct pdf "/D" action with
    | Some dest ->
        begin match rewrite_dest pdf mattable refnumstable dest with
        | Some objnum ->
            Some (Pdf.add_dict_entry action "/D" (Pdf.Indirect objnum))
        | None -> None
        end
    | _ -> None
    end
  | _ -> None
  end

(* For each page, find its annotations. For each, transform its annotations *)
let change_pages_process_annotations mattable refnumstable pdf =
   iter2
     (fun page pnum ->
        (*Pdfe.log (Printf.sprintf "Page %i...\n" pnum);*)
        match Pdf.lookup_direct pdf "/Annots" page.rest with
        | Some (Pdf.Array annots) ->
            iter
              (fun annotobj ->
                match annotobj with Pdf.Indirect i ->
                  let annot = Pdf.lookup_obj pdf i in
                  (* Find its destination, if it has one. Either in /Dest or /A *)
                  begin match Pdf.lookup_direct pdf "/Subtype" annot with
                  | Some (Pdf.Name "/Link") ->
                      begin match Pdf.lookup_direct pdf "/Dest" annot with
                      | Some dest ->
                          begin match rewrite_dest pdf mattable refnumstable dest with
                          | Some objnum ->
                              let new_annot = Pdf.add_dict_entry annot "/Dest" (Pdf.Indirect objnum) in
                                Pdf.addobj_given_num pdf (i, new_annot)
                          | None -> ()
                          end
                      | _ ->
                          begin match Pdf.lookup_direct pdf "/A" annot with
                          | Some (Pdf.Dictionary _ as action) ->
                              begin match rewrite_action pdf mattable refnumstable action with
                              | Some action ->
                                  let new_annot = Pdf.add_dict_entry annot "/A" action in
                                    Pdf.addobj_given_num pdf (i, new_annot)
                              | _ -> ()
                              end
                          | _ -> () 
                          end
                      end
                  | _ -> ()
                  end
                | _ -> Pdfe.log "change_pages_process_annotations: annotation direct\n")
                annots
        | None -> ()
        | _ ->
            Pdfe.log "change_pages_process_annotations: /Annots not an array\n")
     (pages_of_pagetree pdf)
     (indx (pages_of_pagetree pdf))

(* Process the /OpenAction if its destination or action points to a page which has been scaled. *)
(* Trailer --/Root--> Catalog dict --/OpenAction--> (array = dest || dict == action). No name option. *)
let rewrite_openaction pdf action =
  let catalog = Pdf.catalog_of_pdf pdf in
  let catalog = Pdf.add_dict_entry catalog "/OpenAction" action in
    Pdf.addobj_given_num pdf (pdf.Pdf.root, catalog)

let change_pages_process_openaction mattable refnumstable pdf =
  match Pdf.lookup_direct pdf "/OpenAction" (Pdf.catalog_of_pdf pdf) with
  | Some (Pdf.Array dest) ->
      begin match rewrite_dest pdf mattable refnumstable (Pdf.Array dest) with
      | Some new_dest_objnum -> rewrite_openaction pdf (Pdf.Indirect new_dest_objnum)
      | None -> ()
      end
  | Some (Pdf.Dictionary action) ->
      begin match rewrite_action pdf mattable refnumstable (Pdf.Dictionary action) with
      | Some action -> rewrite_openaction pdf action
      | _ -> ()
      end
  | _ -> ()

let change_pages ?matrices ?changes change_references basepdf pages' =
  let pdf = Pdf.empty () in
    Pdf.objiter (fun k v -> ignore (Pdf.addobj_given_num pdf (k, v))) basepdf;
    pdf.Pdf.objects.Pdf.object_stream_ids <- Hashtbl.copy basepdf.Pdf.objects.Pdf.object_stream_ids; (* Preserve objstms 11/12/2021 *)
    let old_page_numbers = Pdf.page_reference_numbers basepdf in
    let pdf, pagetree_num = add_pagetree pages' pdf in
      let pdf =
        {pdf with
           Pdf.major = basepdf.Pdf.major;
           Pdf.minor = basepdf.Pdf.minor;
           Pdf.trailerdict = basepdf.Pdf.trailerdict;
           Pdf.saved_encryption = basepdf.Pdf.saved_encryption}
      in
        let existing_root_entries =
          try
            match Pdf.lookup_obj basepdf basepdf.Pdf.root with
            | Pdf.Dictionary d -> d
            | _ -> []
          with
          _ -> []
        in
          let pdf = add_root pagetree_num existing_root_entries pdf in
            let new_page_numbers = Pdf.page_reference_numbers pdf in
              if not change_references then pdf else
                let changes =
                   match changes with
                     None ->
                       if length old_page_numbers = length new_page_numbers then
                         combine old_page_numbers new_page_numbers
                       else
                         begin
                           Pdfe.log "change_pages: No change supplied, and lengths differ\n";
                           []
                         end
                   | Some cs ->
                       (* Turn the 1-based serial numbers into page reference numbers *)
                       try
                         map
                           (fun (x, y) ->
                            List.nth old_page_numbers (x - 1), List.nth new_page_numbers (y - 1))
                         cs
                       with
                         _ -> raise (Pdf.PDFError "change_pages: bad serial number")
                in
                  Pdf.objselfmap
                    (Pdf.renumber_object_parsed ~preserve_order:false pdf (hashtable_of_dictionary changes))
                    pdf;
                  match matrices with
                    None -> pdf
                  | Some matpairs ->
                      let refnums = Pdf.page_reference_numbers pdf in
                      let mattable = hashtable_of_dictionary matpairs in
                      let refnumstable = hashtable_of_dictionary (combine refnums (indx refnums)) in
                      let pdf =
                        if length old_page_numbers = length new_page_numbers then
                          change_pages_process_bookmarks mattable refnumstable pdf
                        else
                          begin
                            Pdfe.log "Pdfpage.change_pages: non-null matrices when lengths differ\n";
                            pdf
                          end
                      in
                        begin try change_pages_process_annotations mattable refnumstable pdf with
                          e -> Pdfe.log (Printf.sprintf "failure in change_pages_process_annotations: %s\n" (Printexc.to_string e))
                        end;
                        begin try change_pages_process_openaction mattable refnumstable pdf with
                          e -> Pdfe.log (Printf.sprintf "failure in change_pages_process_openaction: %s\n" (Printexc.to_string e))
                        end;
                        pdf

(* Return a pdf with a subset of pages, but nothing else changed - exactly the
same page object numbers, so bookmarks etc still work. Also sorts out bookmarks
so only those in the range are kept. *)

(* Find a page indirect from the page tree of a document, given a page number. *)
let page_object_number pdf destpage =
  try
    Some (select destpage (Pdf.page_reference_numbers pdf))
  with
    (* The page might not exist in the output *)
    Invalid_argument _ (*"select"*) -> None

let target_of_pagenumber pdf i =
  match page_object_number pdf i with
  | None -> Pdfdest.NullDestination
  | Some p -> Pdfdest.Fit (Pdfdest.PageObject p)

(* Build a pagetree using existing object numbers of exisiting pages, adding
any intermediate nodes to the pdf *)
let buildnode kids parent count =
  Pdf.Dictionary
    ([("/Type", Pdf.Name "/Pages");
     ("/Kids", Pdf.Array (map (function i -> Pdf.Indirect i) kids));
     ("/Count", Pdf.Integer count)]
     @
     (if parent = 0 then [] else [("/Parent", Pdf.Indirect parent)]))

let objnumfrom = function
  | OLf (_, _, o) -> o
  | OBr (_, _, _, _, o) -> o

let rec countof = function
  | OLf (os, _, _) -> length os
  | OBr (os, l, r, _, _) -> countof l + countof r + length os

let rec objects_of_ptree_objnumbers pdf = function
  | OLf (objnumbers, parent, objnumofthisleaf) as node ->
      Pdf.addobj_given_num pdf
        (objnumofthisleaf, buildnode objnumbers parent (countof node))
  | OBr (objnumbers, left, right, parent, objnumofthisbranch) as node ->
      Pdf.addobj_given_num pdf
        (objnumofthisbranch, buildnode ([objnumfrom left] @ objnumbers @ [objnumfrom right]) parent (countof node));
      objects_of_ptree_objnumbers pdf left;
      objects_of_ptree_objnumbers pdf right

let pdf_of_pages_build_pagetree thetree objnumbers pdf =
  match thetree with
  | OLf (objnumbers, parent, _) as node ->
      (* Just a leaf. Don't add it, just build the node *)
      buildnode objnumbers parent (countof node)
  | OBr (objnumbers, left, right, parent, _) as node ->
      (* A branch. Return the top level built node, and call main function on left, right *)
      objects_of_ptree_objnumbers pdf left;
      objects_of_ptree_objnumbers pdf right;
      buildnode ([objnumfrom left] @ objnumbers @ [objnumfrom right]) parent (countof node)

(* pdf_of_pages, if it has duplicates in the range, will produce duplicate
items in the page tree, pointing to the same page object. This is bad for
two reasons:
   a) Adobe Reader is broken and crashes in this case
   b) In any event, duplicate references make further document changes
   confusing for most programs.  So, we duplicate the actual page objects, and
   do the minimal renumbering.  *)

(* Given a number n, of a page node, copy it to a new object, and rewrite all
but the first instance in the page tree to that new number. *)
(*exception RewriteDone

(* Rewrite first instance of an indirect in an array of such. *)
let rec rewrite_first_kid m n = function
    [] -> []
  | Pdf.Indirect x::t when x = m -> Pdf.Indirect n :: t
  | h::t -> h :: rewrite_first_kid m n t

(* Rewrite first instance of m if any, in obj to n at objnum. Raise Rewrite if
we did it. *)
let rewrite_first_indirect pdf objnum obj m n =
  match Pdf.lookup_direct pdf "/Kids" obj with
    Some (Pdf.Array kids) ->
      if mem (Pdf.Indirect m) kids then
        let newobj =
          Pdf.add_dict_entry obj "/Kids" (Pdf.Array (rewrite_first_kid m n kids))
        in
          Pdf.addobj_given_num pdf (objnum, newobj);
          raise RewriteDone
  | _ -> failwith "rewrite_first_indirect"

(* Those page tree nodes which are not pages *)
let page_tree_nodes_not_pages pdf =
  let objs = ref [] in
    Pdf.objiter
      (fun objnum o ->
        match o with
          Pdf.Dictionary d when lookup "/Type" d = Some (Pdf.Name "/Pages") ->
            objs := (objnum, o) :: !objs
        | _ -> ())
      pdf;
    !objs

let rewrite_page_tree_first pdf m =
  let n = Pdf.addobj pdf (Pdf.lookup_obj pdf m)
  and nodes = page_tree_nodes_not_pages pdf in
    try
      iter
        (fun (objnum, obj) -> rewrite_first_indirect pdf objnum obj m n)
        nodes
    with
      RewriteDone -> () 
    | _ -> raise (Pdf.PDFError "rewrite_page_tree_first: malformed page tree")

(* Run this strategy repeatedly, until there are no duplicate page objects *)
let rec fixup_duplicate_pages pdf =
  let pagerefs = Pdf.page_reference_numbers pdf in
    let groups =
      keep
        (fun x -> length x > 1)
        (collate compare (sort compare pagerefs))
    in
      match groups with
        (h::_)::_ ->
          rewrite_page_tree_first pdf h;
          fixup_duplicate_pages pdf
      | _ -> ()

(* When there are duplicate pages, even once de-duplicated by
 * fixup_duplicate_pages, we can end up with incorrect /Parent links. This
 * procedure rewrites them. *)
let rec fixup_parents_inner pdf parent_objnum objnum =
  (*Printf.printf "fixup_parents_inner %i %i\n" parent_objnum objnum;*)
  let obj = Pdf.lookup_obj pdf objnum in
    begin match Pdf.indirect_number pdf "/Parent" obj with
      Some _ ->
        Pdf.addobj_given_num pdf (objnum, (Pdf.add_dict_entry obj "/Parent" (Pdf.Indirect parent_objnum)))
    | _ -> ()
    end;
    begin match Pdf.lookup_direct pdf "/Kids" obj with
      Some (Pdf.Array kids) ->
        iter (function Pdf.Indirect x -> fixup_parents_inner pdf objnum x | _ -> ()) kids
    | _ -> ()
    end

let fixup_parents pdf =
  let root = Pdf.lookup_obj pdf pdf.Pdf.root in
    match Pdf.indirect_number pdf "/Pages" root with
      Some pagetreeroot -> fixup_parents_inner pdf 0 pagetreeroot
    | _ -> raise (Pdf.PDFError "fixup_parents: no page tree root")*)

(* New simpler, better procedure. We find all the indirects in the parent tree
   which point to a page and copy objects to rewrite any duplicates (leaving the
   first one alone).  In addition, we must duplicate any annots when copying a
   page, because annots are not allows to be shared between pages. *)
let new_fixup_duplicate_pages pdf = ()

(* Page-nulling will have left us with a /Dests tree with some destinations
   pointing to null pages. This can be a problem when pulling apart and
   re-assembling a single file, where we want destintations to match back up
   upon merging - the null one might get chosen instead. So we rewrite the
   /Dests name tree, removing any name which now points to a nulled page.
   Hopefully a missing name is treated by viewers no worse than a name which
   points to a missing page. *)

(* FIXME /Dests could alternatively be in directly in document catalog, in very old PDFs. *)
(* FIXME Dictionary instead of array in /Dests entry *)
let was_nulled pdf (_, d) =
  match Pdf.direct pdf d with
  | Pdf.Array (Pdf.Indirect i::_) ->
      begin match Pdf.lookup_obj pdf i with
      | Pdf.Null | exception Not_found -> true
      | _ -> false
      end
  | _ -> false

let fixup_destinations pdf =
  match Pdf.lookup_chain pdf pdf.Pdf.trailerdict ["/Root"; "/Names"; "/Dests"] with
  | Some t ->
      let tree = Pdftree.read_name_tree pdf t in
      let tree' = lose (was_nulled pdf) tree in
      let tree'obj = Pdftree.build_name_tree false pdf tree' in
        Pdf.replace_chain pdf ["/Root"; "/Names"; "/Dests"] tree'obj
  | None ->
      ()

(* For all pages in objnumbers, make sure they have no inherited attribues. *)
let replace_inherit pdf objnumbers =
  iter
    (function objnum ->
       let replace_inherit objnum entry =
         let obj = Pdf.lookup_obj pdf objnum in
           (* Find the first parent entry we can which has the correct attribute. *)
           let rec find_attribute obj =
             (* Only replace if not there! *)
             match Pdf.lookup_direct pdf entry obj with
             | Some _ -> None
             | _ ->
               match Pdf.lookup_direct pdf "/Parent" obj with
               | Some (Pdf.Dictionary parent) ->
                   (* Does this one have the attribute? If yes,
                   return, if no carry on looking... Don't use a
                   direct lookup, because we want to retain the
                   indirect reference to resources, for example. *)
                   begin match lookup entry parent with
                   | Some pdfobj -> Some pdfobj
                   | None -> find_attribute (Pdf.Dictionary parent)
                   end
               | _ -> None (* Got to top, couldn't find anything *)
           in
             match find_attribute obj with
             | None -> ()
             | Some replacement_attr ->
                (* Replace the attribute with replacement_attr, updating the page object in place. *)
                Pdf.addobj_given_num pdf (objnum, Pdf.add_dict_entry obj entry replacement_attr)
       in
         replace_inherit objnum "/MediaBox";
         replace_inherit objnum "/CropBox";
         replace_inherit objnum "/Rotate";
         replace_inherit objnum "/Resources")
    objnumbers

let pdf_of_pages ?(retain_numbering = false) ?(process_struct_tree = false) basepdf range =
  if range = [] then raise (Pdf.PDFError "pdf_of_pages: cannot build PDF with no pages") else
  let page_labels =
    if length (Pdfpagelabels.read basepdf) = 0 then [] else
      if retain_numbering
        then Pdfpagelabels.merge_pagelabels [basepdf] [range]
        else []
  and marks =
    let refnums = Pdf.page_reference_numbers basepdf in
    let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
    let table = hashset_of_list range in
    let firstindex = ref ~-1 in
    let firstindexlevel = ref ~-1 in
    let index = ref ~-1 in
    let marks = Pdfmarks.read_bookmarks ~preserve_actions:true basepdf in
    let r =
      option_map
        (function m ->
          index += 1;
          if Hashtbl.mem table (pagenumber_of_target ~fastrefnums basepdf m.Pdfmarks.target) then
            begin
              (*Printf.printf "Keeping index %i: %s\n" !index m.Pdfmarks.text;*)
              if !firstindex = -1 then
                begin
                  firstindex := !index;
                  firstindexlevel := m.Pdfmarks.level
                end;
              Some m
            end
          else
            None)
        marks
    in
      (*Printf.printf "Index of first saved bookmark: %i, at level %i\n" !firstindex !firstindexlevel;*)
      (* Find the last bookmark at firstindexlevel - 1, firstindexlevel -2 .... 0 and add them as prefixes *) 
      firstindexlevel -= 1;
      let got = ref [] in
      for n = !firstindex downto 0 do
        let mark = List.nth marks n in
        (*Printf.printf "n = %i: %s\n" n mark.Pdfmarks.text;*)
        if !firstindexlevel > -1 then
          if mark.Pdfmarks.level = !firstindexlevel then
            begin
              (*Printf.printf "got a mark at level %i\n" !firstindexlevel;*)
              got := mark::!got;
              firstindexlevel -= 1
            end
      done;
      !got @ r
  in
    let pdf = Pdf.empty () in
      Pdf.objiter (fun k v -> ignore (Pdf.addobj_given_num pdf (k, v))) basepdf;
      let page_numbers = Pdf.page_reference_numbers basepdf in
        let pdf =
          {pdf with
             Pdf.major = basepdf.Pdf.major;
             Pdf.minor = basepdf.Pdf.minor;
             Pdf.root = basepdf.Pdf.root; (* So structure tree trimmer can run *)
             Pdf.was_linearized = basepdf.Pdf.was_linearized;
             Pdf.trailerdict = basepdf.Pdf.trailerdict;
             Pdf.saved_encryption = basepdf.Pdf.saved_encryption}
        in
          (*Printf.eprintf "----BEFORE trim_structure_tree\n";
          Pdfwrite.debug_whole_pdf pdf; *)
          if process_struct_tree then Pdfst.trim_structure_tree pdf range;
          (*Printf.eprintf "----AFTER trim_structure_tree\n";
          Pdfwrite.debug_whole_pdf pdf; *)
          let existing_root_entries =
            try
              match Pdf.lookup_obj basepdf basepdf.Pdf.root with | Pdf.Dictionary d -> d | _ -> []
            with
              _ -> []
          in
            let objnumbers = map (function i -> select i page_numbers) range in
            let old_pagetree_root_num =
              match Pdf.lookup_direct basepdf "/Root" pdf.Pdf.trailerdict with
              | Some (Pdf.Dictionary d) ->
                  begin match lookup "/Pages" d with
                  | Some (Pdf.Indirect i) -> i
                  | _ -> raise (Pdf.PDFError "pdf_of_pages")
                  end
              | _ -> raise (Pdf.PDFError "pdf_of_pages")
            in
            (* 1. Look through all the page objects to be included, and
            replicate inheritable entries from their parent nodes, since they
            may fail to exist, leaving pages without Media boxes or
            resources! Inheritable entries are /MediaBox /CropBox /Rotate
            /Resources *)
            replace_inherit pdf objnumbers;
            let thetree = pagetree_with_objnumbers true old_pagetree_root_num (source pdf.Pdf.objects.Pdf.maxobjnum) objnumbers 0 in
            (* 2. Kill the old page tree, excepting pages which will appear in the new
            PDF. It will link, via /Parent entries etc, to the new page tree. To do
            this, we remove all objects with /Type /Page or /Type /Pages. The other
            places that null can appear, in destinations and so on, are ok, we think. *)
            Pdf.objiter
              (fun i o ->
                match o with
                | Pdf.Dictionary d ->
                    begin match lookup "/Type" d with
                    | Some (Pdf.Name ("/Pages")) -> Pdf.removeobj pdf i
                    | Some (Pdf.Name ("/Page")) ->
                        if not (mem i objnumbers) then Pdf.removeobj pdf i
                    | _ -> ()
                    end
                | _ -> ())
              pdf;
              (* Now, add the new page tree, with root at the same object
              number, and finish *)
              let new_pagetree = pdf_of_pages_build_pagetree thetree objnumbers pdf in
                Pdf.addobj_given_num pdf (old_pagetree_root_num, new_pagetree);
                  let pdf = add_root old_pagetree_root_num existing_root_entries pdf in
                  Pdfpagelabels.write pdf page_labels;
                  let pdf = Pdfmarks.add_bookmarks marks pdf in
                    (*fixup_duplicate_pages pdf;
                    fixup_parents pdf;*)
                    new_fixup_duplicate_pages pdf;
                    fixup_destinations pdf;
                    pdf

let prepend_operators pdf ops ?(fast=false) page =
  if fast then
    {page with content =
       Pdfops.stream_of_ops ops :: page.content}
  else
    let old_ops =
      Pdfops.parse_operators pdf page.resources page.content
    in
      {page with content =
        [Pdfops.stream_of_ops (ops @ old_ops)]}

(* Add stack operators to a content stream to ensure it is composeable. *)
let protect ops =
  let qs = length (keep (eq Pdfops.Op_q) ops)
  and bigqs = length (keep (eq Pdfops.Op_Q) ops) in
  let deficit = if qs > bigqs then qs - bigqs else 0 in
    if deficit <> 0 then Pdfe.log (Printf.sprintf "Q Deficit was nonzero. Fixing. %i\n" deficit);
    many Pdfops.Op_Q deficit

(* We check for q/Q mismatches in existing section. *)
let postpend_operators pdf ops ?(fast=false) page =
  if fast then
    {page with content =
       page.content @ [Pdfops.stream_of_ops ([Pdfops.Op_q] @ ops @ [Pdfops.Op_Q])]}
  else
    let existing_ops = Pdfops.parse_operators pdf page.resources page.content in
      if existing_ops = [] then
        {page with content = [Pdfops.stream_of_ops ops]} (* avoid protecting the empty contents *)
      else
        let protected = protect existing_ops in
        let beforeops = [Pdfops.Op_q] in
        let afterops = protected @ [Pdfops.Op_Q] @ ops in
          {page with content =
             [Pdfops.stream_of_ops (beforeops @ existing_ops @ afterops)]}

(* Source of possible prefix strings. String is always copied. *)
let next_string s =
  if s = "" then "a" else
    if s.[0] = 'z' then "a" ^ s else
      String.mapi
        (fun i c ->
           if i = 0 then char_of_int (int_of_char c + 1)
           else c)
        s

(* True if one string [p] is a prefix of another [n] *)
let is_prefix p n =
  String.length p <= String.length n &&
  String.sub n 0 (String.length p) = p

(* a) List every name used in a /Resources in a /Type /Page or
 /Type /Pages (without the leading "/"
   b) Find the shortest lower-case alphabetic string which is not a prefix of any of these
   strings. This prefix can be added to the other PDF's names, and will never
   clash with any of these. *)
let names_used pdf =
  let names = ref [] in
  let unslash x =
    if x = "" then "" else String.sub x 1 (String.length x - 1)
  in
    Pdf.objiter
      (fun n obj ->
        match obj with
          Pdf.Dictionary d | Pdf.Stream {contents = (Pdf.Dictionary d, _)} ->
            begin match lookup "/Type" d with
              Some (Pdf.Name ("/Page" | "/Pages")) ->
                begin match Pdf.lookup_direct pdf "/Resources" obj with
                  Some resources ->
                    iter
                      (fun key ->
                         match Pdf.lookup_direct pdf key resources with
                           Some (Pdf.Dictionary d) ->
                             iter
                               (fun (k, _) -> names := unslash k::!names)
                               d
                         | _ -> ())
                      resource_keys
                | _ -> ()
                end
            | _ -> ()
            end
        | _ -> ()
      )
      pdf;
    setify !names

let shortest names =
  let rec loop prefix =
    if List.exists (is_prefix prefix) names
      then loop (next_string prefix)
      else prefix
  in
    loop "a"

let shortest_unused_prefix pdf =
  shortest (names_used pdf)

let addp p n =
  if n = "" then raise (Pdf.PDFError "addp: blank name") else
    "/" ^ p ^ String.sub n 1 (String.length n - 1)

let direct_cs_names =
  ["/DeviceGray"; "/DeviceRGB"; "/DeviceCMYK"; "/Pattern"]

let direct_cs_names_inline =
  ["/DeviceGray"; "/DeviceRGB"; "/DeviceCMYK"; "/G"; "/RGB"; "/CMYK"]

let prefix_operator pdf p = function
  | Pdfops.Op_Tf (f, s) -> Pdfops.Op_Tf (addp p f, s)
  | Pdfops.Op_gs n -> Pdfops.Op_gs (addp p n)
  | Pdfops.Op_CS n -> Pdfops.Op_CS (if mem n direct_cs_names then n else addp p n)
  | Pdfops.Op_cs n -> Pdfops.Op_cs (if mem n direct_cs_names then n else addp p n)
  | Pdfops.Op_SCNName (s, ns) -> Pdfops.Op_SCNName (addp p s, ns)
  | Pdfops.Op_scnName (s, ns) -> Pdfops.Op_scnName (addp p s, ns)
  | Pdfops.Op_sh s -> Pdfops.Op_sh (addp p s)
  | Pdfops.Op_Do x -> Pdfops.Op_Do (addp p x)
  | Pdfops.Op_DP (n, Pdf.Name x) -> Pdfops.Op_DP (n, Pdf.Name (addp p x))
  | Pdfops.Op_BDC (n, Pdf.Name x) -> Pdfops.Op_BDC (n, Pdf.Name (addp p x))
  | Pdfops.InlineImage (dict, dp, bytes) ->
      (* Replace any indirect "/CS" or "/ColorSpace" with a new "/CS" *)
      let dict' =
        match Pdf.lookup_direct_orelse pdf "/CS" "/ColorSpace" dict with
        | Some (Pdf.Name n) when mem n direct_cs_names_inline -> dict
        | Some (Pdf.Name n) ->
            Pdf.add_dict_entry
              (Pdf.remove_dict_entry
                (Pdf.remove_dict_entry dict "/ColorSpace")
                "/CS")
              "/CS"
              (Pdf.Name (addp p n))
        | _ -> dict
      in
        Pdfops.InlineImage (dict', dp, bytes)
  | x -> x

let change_resources pdf prefix resources =
  let newdict name =
    match Pdf.lookup_direct pdf name resources with
    | Some (Pdf.Dictionary dict) ->
        Pdf.Dictionary (map (fun (k, v) -> addp prefix k, v) dict)
    | _ -> Pdf.Dictionary []
  in
    let newdicts = map newdict resource_keys in
      let resources = ref resources in
        iter2
          (fun k v ->
            resources := Pdf.add_dict_entry !resources k v)
          resource_keys
          newdicts;
        !resources

(* For each object in the PDF with /Type /Page or /Type /Pages:
  a) Add the prefix to any name in /Resources
  b) Add the prefix to any name used in any content streams, keeping track of
  the streams we have processed to preserve sharing *)
let add_prefix pdf prefix =
  let fixed_streams = Hashtbl.create 100 in
  let fix_stream resources i =
    match i with Pdf.Indirect i ->
      if not (Hashtbl.mem fixed_streams i) then
        let operators = Pdfops.parse_operators pdf resources [Pdf.Indirect i] in
          let operators' = map (prefix_operator pdf prefix) operators in
            begin match Pdf.lookup_obj pdf i with
              Pdf.Stream ({contents = (dict, stream)} as s) ->
                begin match Pdfops.stream_of_ops operators' with
                  Pdf.Stream {contents = ncontents} -> s := ncontents
                | _ -> failwith "add_prefix: bad stream"
                end
            | _ -> failwith "add_prefix: bad stream 2"
            end;
            Hashtbl.add fixed_streams i ()
    | _ -> failwith "add_prefix: not indirect"
  in
  Pdf.objselfmap
    (fun obj ->
       match obj with
         Pdf.Dictionary dict as d ->
           begin match Pdf.lookup_direct pdf "/Type" d with
             Some (Pdf.Name ("/Page" | "/Pages")) ->
               let resources, resources' =
                 begin match Pdf.lookup_direct pdf "/Resources" obj with
                   Some resources -> Some resources, Some (change_resources pdf prefix resources)
                 | _ -> None, None
                 end
               in
                 begin match lookup "/Contents" dict with
                   Some (Pdf.Indirect i) ->
                     fix_stream
                       (if resources = None then Pdf.Dictionary [] else unopt resources)
                       (Pdf.Indirect i)
                 | Some (Pdf.Array a) ->
                     (* May be Non-ISO, and not parse properly individually! If so, detect, and Exit *)
                     begin match
                       if length a > 1 then (* Can't be broken when just a single entry *)
                         iter
                           (function c ->
                             let c = Pdf.direct pdf c in
                               Pdfcodec.decode_pdfstream pdf c;
                               let s = Pdf.bigarray_of_stream c in
                               let resources = match resources with Some r -> r | _ -> Pdf.Dictionary [] in
                                 ignore (Pdfops.parse_single_stream pdf resources s))
                           a
                     with
                     | () ->
                         iter
                           (fix_stream
                             (if resources = None then Pdf.Dictionary [] else unopt resources))
                           a
                     | exception _ ->
                         Pdfe.log "add_prefix: non-ISO PDF detected streams detected. Fixing...\n";
                         raise Exit
                     end;
                 | _ -> ()
                 end;
                 begin match resources' with
                   Some x -> Pdf.add_dict_entry d "/Resources" x
                 | None -> d
                 end
           | _ -> obj
           end
       | _ -> obj)
    pdf

let merge_content_streams pdf = 
  Pdf.objselfmap
    (fun obj ->
       match obj with
         Pdf.Dictionary dict as d ->
           begin match Pdf.lookup_direct pdf "/Type" d with
             Some (Pdf.Name ("/Page" | "/Pages")) ->
               begin match lookup "/Contents" dict with
               | Some (Pdf.Indirect _ | Pdf.Array [_]) -> obj
               | Some (Pdf.Array a) ->
                   let a = map (Pdf.direct pdf) a in
                     iter (Pdfcodec.decode_pdfstream pdf) a;
                     let bigarrays = map Pdf.bigarray_of_stream a in
                     let merged = Pdfops.concat_bytess bigarrays in
                     let stream =
                       Pdf.Stream
                         {contents =
                            (Pdf.Dictionary ["/Length", Pdf.Integer (Pdfio.bytes_size merged)],
                             Pdf.Got merged)}
                     in
                     let i = Pdf.addobj pdf stream in
                       Pdf.add_dict_entry d "/Contents" (Pdf.Indirect i)
               | _ -> obj
               end
           | _ -> obj
           end
       | _ -> obj)
   pdf

(* If a non-ISO PDF with content streams which don't end on lexical boundaries
is provided, we must merge them. But we don't want to unless we have to,
because it destroys sharing. *)
let add_prefix pdf prefix =
  try add_prefix pdf prefix with Exit ->
    merge_content_streams pdf;
    add_prefix pdf prefix

(* For uses of process_pages which don't need to deal with matrices, this
function transforms into one which returns the identity matrix *)
let ppstub f n p = (f n p, n, Pdftransform.i_matrix)

let process_xobject f pdf resources i =
  let xobj = Pdf.lookup_obj pdf i in
    match Pdf.lookup_direct pdf "/Subtype" xobj with
    | None -> raise (Pdf.PDFError "No /Subtype in Xobject") 
    | Some (Pdf.Name "/Form") ->
        Pdf.getstream xobj;
        begin match xobj with
        | Pdf.Stream ({contents = Pdf.Dictionary dict, Pdf.Got bytes} as rf) ->
            begin match f pdf resources [Pdf.Stream rf] with
            | [Pdf.Stream {contents = (Pdf.Dictionary dict', data)}] ->
                let dict' =
                  Pdf.remove_dict_entry
                    (Pdf.Dictionary (mergedict dict dict'))
                    "/Filter"
                in
                  rf := (dict', data)
            | _ -> assert false
            end
        | _ -> assert false (* getstream would have complained already *)
        end
    | Some _ -> ()

let process_xobjects pdf page f =
  match Pdf.lookup_direct pdf "/XObject" page.resources with
  | Some (Pdf.Dictionary elts) ->
      iter
        (fun (k, v) ->
          match v with
          | Pdf.Indirect i -> process_xobject f pdf page.resources i
          | _ -> raise (Pdf.PDFError "process_xobject"))
        elts
  | _ -> ()

(* Union two resource dictionaries from the same PDF. *)
let combine_pdf_resources pdf a b =
  let a_entries =
    match a with
    | Pdf.Dictionary entries -> entries
    | _ -> []
  in let b_entries =
    match b with
    | Pdf.Dictionary entries -> entries
    | _ -> []
  in
    let resource_keys =
      ["/Font"; "/ExtGState"; "/ColorSpace"; "/Pattern";
       "/Shading"; "/XObject"; "/Properties"]
    in
      let combine_entries key =
        let a_entries =
          match Pdf.lookup_direct pdf key a with
          | Some (Pdf.Dictionary d) -> d
          | _ -> []
        in let b_entries =
          match Pdf.lookup_direct pdf key b with
          | Some (Pdf.Dictionary d) -> d
          | _ -> []
        in
          if a_entries = [] && b_entries = [] then
            None
          else
            Some (key, Pdf.Dictionary (a_entries @ b_entries))
      in
        let unknown_keys_a = lose (fun (k, _) -> mem k resource_keys) a_entries in
        let unknown_keys_b = lose (fun (k, _) -> mem k resource_keys) b_entries in
        let combined_known_entries = option_map combine_entries resource_keys in
          fold_left
            (fun dict (k, v) -> Pdf.add_dict_entry dict k v)
            (Pdf.Dictionary [])
            (unknown_keys_a @ unknown_keys_b @ combined_known_entries)

let minimum_valid_pdf () =
  let pdf_pages = [blankpage Pdfpaper.a4] in
  let pdf, pageroot = add_pagetree pdf_pages (Pdf.empty ()) in
    add_root pageroot [] pdf
