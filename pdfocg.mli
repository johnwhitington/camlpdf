(** Optional Content Groups. *)

open Pdfutil

type ocgusage =
  {ocg_creatorinfo : Pdf.t option;
   ocg_language : Pdf.t option;
   ocg_export : Pdf.t option;
   ocg_zoom : Pdf.t option;
   ocg_print : Pdf.t option;
   ocg_view : Pdf.t option;
   ocg_user : Pdf.t option;
   ocg_pageelement : Pdf.t option}

type ocg =
  {ocg_name : string;
   ocg_intent : string list;
   ocg_usage : ocgusage option}

type ocgstate = OCG_ON | OCG_OFF | OCG_Unchanged

type ocglistmode = OCG_AllPages | OCG_VisiblePages

type ocgevent = OCG_View | OCG_Print | OCG_Export

type ocgappdict =
  {ocg_event : ocgevent;
   ocg_ocgs : string list;
   ocg_category : string list}

type ocgconfig =
  {ocgconfig_name : string option;
   ocgconfig_creator : string option;
   ocgconfig_basestate : ocgstate;
   ocgconfig_on : int list option;
   ocgconfig_off : int list option;
   ocgconfig_intent: string list;
   ocgconfig_usage_application_dictionaries: ocgappdict list option;
   ocgconfig_order : int list option;
   ocgconfig_listmode : ocglistmode;
   ocgconfig_rbgroups : int list list option;
   ocgconfig_locked : int list}

type t =
  {ocgs : (int * ocg) list;
   ocg_default_config : ocgconfig;
   ocg_configs : ocgconfig list option}

(** Read optional content data. *)
val read_ocg : Pdf.t -> t option

(** Write optional content data. *)
val write_ocg : Pdf.t -> t -> unit
