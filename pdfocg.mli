(** Optional Content Groups. *)

open Pdfutil

type ocgusage =
  {ocg_creatorinfo : (string * string) option;
   ocg_language : (string  * string option) option;
   ocg_export : string option;
   ocg_zoom_min : float option;
   ocg_zoom_max : float option;
   ocg_print_subtype : string option;
   ocg_print_printstate : string option;
   ocg_viewstate : string option;
   ocg_user : (string * string list) option;
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
   ocgconfig_usage_application_dictionaries: ocgappdict list;
   ocgconfig_order : (string option * int list) list option;
   ocgconfig_listmode : ocglistmode;
   ocgconfig_rbgroups : int list list option;
   ocgconfig_locked : int list}

type t =
  {ocgs : (int * ocg) list;
   ocg_default_config : ocgconfig;
   ocg_configs : ocgconfig list}

(** Read optional content data. *)
val read_ocg : Pdf.t -> t option

(** Write optional content data. *)
val write_ocg : Pdf.t -> t -> unit
