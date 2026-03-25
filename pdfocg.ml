(* Optional Content Groups *)
open Pdfutil

type ocgusage =
  {ocg_creatorinfo : (string * string) option;
   ocg_language : (string * string option) option;
   ocg_export : string option;
   ocg_zoom_min : float option;
   ocg_zoom_max : float option;
   ocg_print_subtype : string option;
   ocg_print_printstate : string option;
   ocg_viewstate : string option;
   ocg_user : (string  * string list) option;
   ocg_page_element_subtype : string option}

type ocg =
  {ocg_name : string;
   ocg_intent : string list;
   ocg_usage : ocgusage option}

type ocgstate = OCG_ON | OCG_OFF | OCG_Unchanged

type ocglistmode = OCG_AllPages | OCG_VisiblePages

type ocgevent = OCG_View | OCG_Print | OCG_Export

type ocgappdict =
  {ocg_event : ocgevent;
   ocg_ocgs : int list;
   ocg_category : string list}

type ocgconfig =
  {ocgconfig_name : string option;
   ocgconfig_creator : string option;
   ocgconfig_basestate : ocgstate;
   ocgconfig_on : int list;
   ocgconfig_off : int list;
   ocgconfig_intent: string list;
   ocgconfig_usage_application_dictionaries : ocgappdict list;
   ocgconfig_order : (string option * int list) list option;
   ocgconfig_listmode : ocglistmode;
   ocgconfig_rbgroups : int list list option;
   ocgconfig_locked : int list}

type t =
  {ocgs : (int * ocg) list;
   ocg_default_config : ocgconfig;
   ocg_configs : ocgconfig list}

let read_ocgappdict pdf appdict =
  let ocg_event =
    match Pdf.lookup_direct pdf "/Event" appdict with
    | Some (Pdf.Name "/View") -> OCG_View
    | Some (Pdf.Name "/Print") -> OCG_Print
    | Some (Pdf.Name "/Export") -> OCG_Export
    | _ -> OCG_View
  in
  let ocg_ocgs = 
    match Pdf.lookup_direct pdf "/OCGs" appdict with
    | Some (Pdf.Array l) -> map (function Pdf.Indirect i -> i | _ -> 0) l
    | _ -> []
  in
  let ocg_category =
    match Pdf.lookup_direct pdf "/Category" appdict with
    | Some (Pdf.Array l) -> map (function Pdf.Name n -> n | _ -> "") (map (Pdf.direct pdf) l)
    | _ -> []
  in
   {ocg_event; ocg_ocgs; ocg_category}

let read_order pdf = function
  | Pdf.Array a ->
      map
        (function
          | Pdf.Indirect i -> (None, [i])
          | Pdf.Array (Pdf.String s::r) -> (Some s, map (function Pdf.Indirect i -> i | _ -> 0) r)
          | Pdf.Array a -> (None, map (function Pdf.Indirect i -> i | _ -> 0) a)
          | _ -> (None, []))
        a
  | _ ->
      Pdfe.log "Bad /Order";
      []

let read_config pdf config =
  let ocgconfig_name =
    match Pdf.lookup_direct pdf "/Name" config with
    | Some (Pdf.String s) -> Some s
    | _ -> None
  and ocgconfig_creator =
    match Pdf.lookup_direct pdf "/Creator" config with
    | Some (Pdf.String s) -> Some s
    | _ -> None
  and ocgconfig_basestate =
    match Pdf.lookup_direct pdf "/BaseState" config with
    | Some (Pdf.Name "/ON") -> OCG_ON
    | Some (Pdf.Name "/OFF") -> OCG_OFF
    | Some (Pdf.Name "/Unchanged") -> OCG_Unchanged
    | _ -> OCG_ON
  and ocgconfig_on =
    match Pdf.lookup_direct pdf "/ON" config with
    | Some (Pdf.Array indirects) -> 
        map (function Pdf.Indirect n -> n | _ -> 0) indirects
    | _ -> []
  and ocgconfig_off =
    match Pdf.lookup_direct pdf "/OFF" config with
    | Some (Pdf.Array indirects) -> 
        map (function Pdf.Indirect n -> n | _ -> 0) indirects
    | _ -> []
  and ocgconfig_intent =
    match Pdf.lookup_direct pdf "/Intent" config with
    | Some (Pdf.Name n) -> [n]
    | Some (Pdf.Array a) ->
        (map
          (function x ->
             match Pdf.direct pdf x with | Pdf.Name n -> n | _ -> "")
          a)
    | _ -> ["/View"]
  and ocgconfig_usage_application_dictionaries =
    match Pdf.lookup_direct pdf "/AS" config with
    | Some (Pdf.Array appdicts) -> map (read_ocgappdict pdf) appdicts
    | _ -> []
  and ocgconfig_order =
    match Pdf.lookup_direct pdf "/Order" config with
    | Some order -> Some (read_order pdf order)
    | None -> None
  and ocgconfig_listmode =
    match Pdf.lookup_direct pdf "/ListMode" config with
    | Some (Pdf.Name "/VisiblePages") -> OCG_VisiblePages
    | _ -> OCG_AllPages
  and ocgconfig_rbgroups =
    match Pdf.lookup_direct pdf "/RBGroups" config with
    | Some (Pdf.Array l) ->
        Some (map (function Pdf.Array l -> map (function Pdf.Indirect i -> i | _ -> 0) l | _ -> []) l)
    | _ -> None
  and ocgconfig_locked = 
    match Pdf.lookup_direct pdf "/Locked" config with
    | Some (Pdf.Array a) ->
          (map
            (function x ->
               match Pdf.direct pdf x with | Pdf.Indirect n -> n | _ -> 0)
            a)
    | _ -> []
  in
    {ocgconfig_name;
     ocgconfig_creator;
     ocgconfig_basestate;
     ocgconfig_on;
     ocgconfig_off;
     ocgconfig_intent;
     ocgconfig_usage_application_dictionaries;
     ocgconfig_order;
     ocgconfig_listmode;
     ocgconfig_rbgroups;
     ocgconfig_locked}

let read_ocg_usage pdf usage =
  let ocg_creatorinfo_creator =
    match Pdf.lookup_chain pdf usage ["/CreatorInfo"; "/Creator"] with
    | Some (Pdf.String s) -> Some s
    | _ -> None
  in
  let ocg_creatorinfo_subtype =
    match Pdf.lookup_chain pdf usage ["/CreatorInfo"; "/Subtype"] with
    | Some (Pdf.Name s) -> Some s
    | _ -> None
  in
  let ocg_creatorinfo =
    match ocg_creatorinfo_creator, ocg_creatorinfo_subtype with
    | None, None -> None
    | Some a, None -> Some (a, "")
    | None, Some b -> Some ("", b)
    | Some a, Some b -> Some (a, b)
  in
  let ocg_language_lang =
    match Pdf.lookup_chain pdf usage ["/Language"; "/Lang"] with
    | Some (Pdf.String s) -> Some s
    | _ -> None
  in
  let ocg_language_preferred =
    match Pdf.lookup_chain pdf usage ["/Language"; "/Preferred"] with
    | Some (Pdf.Name s) -> Some s
    | _ -> None
  in
  let ocg_language =
    match ocg_language_lang, ocg_language_preferred with
    | None, _ -> None
    | Some a, b -> Some (a, b)
  in
  let ocg_export =
    match Pdf.lookup_chain pdf usage ["/Export"; "/ExportState"] with
    | Some (Pdf.Name s) -> Some s
    | _ -> None
  in
  let ocg_zoom_min =
    match Pdf.lookup_chain pdf usage ["/Zoom"; "/min"] with
    | Some x -> Some (Pdf.getnum pdf x)
    | _ -> None
  in
  let ocg_zoom_max =
    match Pdf.lookup_chain pdf usage ["/Zoom"; "/max"] with
    | Some x -> Some (Pdf.getnum pdf x)
    | _ -> None
  in
  let ocg_print_subtype = 
    match Pdf.lookup_chain pdf usage ["/Print"; "/Subtype"] with
    | Some (Pdf.Name s) -> Some s
    | _ -> None
  in
  let ocg_print_printstate =
    match Pdf.lookup_chain pdf usage ["/Print"; "/PrintState"] with
    | Some (Pdf.Name s) -> Some s
    | _ -> None
  in
  let ocg_viewstate =
    match Pdf.lookup_chain pdf usage ["/View"; "/ViewState"] with
    | Some (Pdf.Name s) -> Some s
    | _ -> None
  in
  let ocg_user_type =
    match Pdf.lookup_chain pdf usage ["/User"; "/Type"] with
    | Some (Pdf.Name s) -> Some s
    | _ -> None
  in
  let ocg_user_name =
    match Pdf.lookup_chain pdf usage ["/User"; "/Name"] with
    | Some (Pdf.String s) -> Some [s]
    | Some (Pdf.Array a) -> Some (map (function Pdf.String s -> s | _ -> "") (map (Pdf.direct pdf) a))
    | _ -> None
  in
  let ocg_user =
    match ocg_user_type, ocg_user_name with
    | Some a, Some b -> Some (a, b)
    | _ -> None
  in
  let ocg_page_element_subtype =
    match Pdf.lookup_chain pdf usage ["/PageElement"; "/Subtype"] with
    | Some (Pdf.Name s) -> Some s
    | _ -> None
  in
    {ocg_creatorinfo;
     ocg_language;
     ocg_export;
     ocg_zoom_min;
     ocg_zoom_max;
     ocg_print_subtype;
     ocg_print_printstate;
     ocg_viewstate;
     ocg_user;
     ocg_page_element_subtype}

let read_individual_ocg pdf ocg =
  let ocg_name =
    match Pdf.lookup_direct pdf "/Name" ocg with
    | Some (Pdf.String s) -> s
    | _ -> raise (Pdf.PDFError "No /Name in optional content group")
  and ocg_intent =
    match Pdf.lookup_direct pdf "/Intent" ocg with
    | Some (Pdf.Name n) -> [n]
    | Some (Pdf.Array a) ->
       option_map (function Pdf.Name n -> Some n | _ -> None) (map (Pdf.direct pdf) a)
    | _ -> ["/View"]
  and ocg_usage =
    match Pdf.lookup_direct pdf "/Usage" ocg with
    | None -> None
    | Some usage -> Some (read_ocg_usage pdf usage)
  in
    {ocg_name = ocg_name;
     ocg_intent = ocg_intent;
     ocg_usage = ocg_usage}

let read_ocg pdf =
  match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
  | None -> raise (Pdf.PDFError "No /Root")
  | Some r ->
      match Pdf.lookup_direct pdf "/OCProperties" r with
      | None -> None
      | Some ocproperties ->
          let ocgs =
            match Pdf.lookup_direct pdf "/OCGs" ocproperties with
            | Some (Pdf.Array indirects) ->
               combine
                 (map (function Pdf.Indirect n -> n | _ -> 0) indirects)
                 (map (read_individual_ocg pdf) indirects)
            | _ -> []
          and ocg_default_config =
            match Pdf.lookup_direct pdf "/D" ocproperties with
            | None -> raise (Pdf.PDFError "No default config in /OCProperties")
            | Some config -> read_config pdf config
          and ocg_configs =
            match Pdf.lookup_direct pdf "/Configs" ocproperties with
            | Some (Pdf.Array configs) -> map (read_config pdf) configs
            | _ -> []
          in
            Some
              {ocgs = ocgs;
               ocg_default_config;
               ocg_configs;}

let write_ocg_appdict appdict =
  Pdf.Dictionary
    [("/Event", Pdf.Name (match appdict.ocg_event with OCG_View -> "/View" | OCG_Print -> "/Print" | OCG_Export -> "/Export"));
     ("/OCGs", Pdf.Array (map (fun x -> Pdf.Integer x) appdict.ocg_ocgs));
     ("/Category", Pdf.Array (map (fun x -> Pdf.Name x) appdict.ocg_category))]

let write_ocg_usage pdf usage =
  let creatorinfo =
    match usage.ocg_creatorinfo with
    | None -> []
    | Some (a, b) -> [("/CreatorInfo", Pdf.Dictionary [("/Creator", Pdf.String a); ("/Subtype", Pdf.Name b)])]
  in
  let language =
    match usage.ocg_language with
    | None -> []
    | Some (a, None) -> [("/Language", Pdf.Dictionary [("/Lang", Pdf.String a)])]
    | Some (a, Some b) -> [("/Language", Pdf.Dictionary [("/Lang", Pdf.String a); ("/Preferred", Pdf.Name b)])]
  in
  let export = 
    match usage.ocg_export with
    | None -> []
    | Some a -> [("/Export", Pdf.Dictionary [("/ExportState", Pdf.Name a)])]
  in
  let zoom =
    match usage.ocg_zoom_min, usage.ocg_zoom_max with
    | None, None -> []
    | Some a, None -> [("/Zoom", Pdf.Dictionary [("/min", Pdf.Real a)])]
    | None, Some b -> [("/Zoom", Pdf.Dictionary [("/max", Pdf.Real b)])]
    | Some a, Some b -> [("/Zoom", Pdf.Dictionary [("/min", Pdf.Real a); ("/max", Pdf.Real b)])]
  in
  let print =
    match usage.ocg_print_subtype, usage.ocg_print_printstate with
    | None, None -> []
    | Some a, None -> [("/Print", Pdf.Dictionary [("/Subtype", Pdf.Name a)])]
    | None, Some b -> [("/Print", Pdf.Dictionary [("/PrintState", Pdf.Name b)])]
    | Some a, Some b -> [("/Print", Pdf.Dictionary [("/Subtype", Pdf.Name a); ("/PrintState", Pdf.Name b)])]
  in
  let view =
    match usage.ocg_viewstate with
    | None -> []
    | Some a -> [("/View", Pdf.Dictionary [("/ViewState", Pdf.Name a)])]
  in
  let user =
    match usage.ocg_user with
    | None -> []
    | Some (a, b) ->
        [("/User",
          Pdf.Dictionary [("/Type", Pdf.Name a); ("/Name", Pdf.Array (map (fun x -> Pdf.String x) b))])]
  in
  let pageelement =
    match usage.ocg_page_element_subtype with
    | None -> []
    | Some x -> [("/PageElement", Pdf.Dictionary [("/Subtype", Pdf.Name x)])]
  in
    Pdf.addobj pdf
      (Pdf.Dictionary
         (creatorinfo @ language @ export @ zoom @ print @ view @ user @ pageelement))

let write_ocg_config pdf u =
  let name =
    match u.ocgconfig_name with
    | None -> []
    | Some x -> [("/Name", Pdf.String x)]
  in
  let creator =
    match u.ocgconfig_creator with
    | None -> []
    | Some x -> [("/Creator", Pdf.String x)]
  in
  let basestate =
    match u.ocgconfig_basestate with
    | OCG_ON -> []
    | x -> [("/BaseState", Pdf.Name ((function OCG_OFF -> "/OFF" | _ -> "/Unchanged") x))]
  in
  let on =
    match u.ocgconfig_on with
    | [] -> []
    | ocgs -> [("/ON", Pdf.Array (map (fun x -> Pdf.Indirect x) ocgs))]
  in
  let off =
    match u.ocgconfig_off with
    | [] -> []
    | ocgs -> [("/OFF", Pdf.Array (map (fun x -> Pdf.Indirect x) ocgs))]
  in
  let intent =
    match u.ocgconfig_intent with 
    | [] -> []
    | l -> [("/Intent", Pdf.Array (map (fun x -> Pdf.Name x) l))]
  in
  let usage_application_dictionaries =
    match u.ocgconfig_usage_application_dictionaries with
    | [] -> []
    | l -> [("/AS", Pdf.Array (map write_ocg_appdict l))]
  in
  let order =
    match u.ocgconfig_order with
    | None -> []
    | Some l ->
        let f (so, ints) =
          match so with
          | None -> Pdf.Array (map (fun x -> Pdf.Indirect x) ints)
          | Some s -> Pdf.Array (Pdf.String s::map (fun x -> Pdf.Indirect x) ints)
        in
          [("/Order", Pdf.Array (map f l))]
  in
  let listmode =
    match u.ocgconfig_listmode with
    | OCG_AllPages -> []
    | OCG_VisiblePages -> [("/ListMode", Pdf.Name "/VisiblePages")]
  in
  let rbgroups =
    match u.ocgconfig_rbgroups with
    | None -> []
    | Some l -> [("/RBGroups", Pdf.Array (map (fun l -> Pdf.Array (map (fun x -> Pdf.Indirect x) l)) l))]
  in
  let locked =
    match u.ocgconfig_locked with
    | [] -> []
    | l -> [("/Locked", Pdf.Array (map (fun x -> Pdf.Indirect x) l))]
  in
    Pdf.addobj pdf
      (Pdf.Dictionary
        (name @ creator @ basestate @ on @ off @ intent @ usage_application_dictionaries @
         order @ listmode @ rbgroups @ locked))

let write_ocg_ocg pdf ocg =
  Pdf.Dictionary
    ([("/Name", Pdf.String ocg.ocg_name);
      ("/Intent", Pdf.Array (map (fun x -> Pdf.Name x) ocg.ocg_intent))]
     @
      (match ocg.ocg_usage with None -> [] | Some u -> [("/Usage", Pdf.Indirect (write_ocg_usage pdf u))]))

let write_ocg pdf {ocgs; ocg_default_config; ocg_configs} =
  let ocgs =
    Pdf.addobj pdf (Pdf.Array (map (function (i, ocg) -> Pdf.addobj_given_num pdf (i, write_ocg_ocg pdf ocg); Pdf.Indirect i) ocgs))
  in
  let default =
    write_ocg_config pdf ocg_default_config
  in
  let config =
    Pdf.addobj pdf (Pdf.Array (map (fun x -> Pdf.Indirect (write_ocg_config pdf x)) ocg_configs))
  in
  let ocpropsdict =
    Pdf.Dictionary
      [("/OCGs", Pdf.Indirect ocgs);
       ("/D", Pdf.Indirect default);
       ("/Config", Pdf.Indirect config)]
  in
    Pdf.replace_chain pdf ["/Root"; "/OCProperties"] (Pdf.Indirect (Pdf.addobj pdf ocpropsdict))
