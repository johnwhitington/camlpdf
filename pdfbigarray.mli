(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*         Manuel Serrano and Xavier Leroy, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(** Specialized bigarray for CamlPDF *)

type int8_unsigned_elt

type ('a, 'b) kind

val int8_unsigned : (int, int8_unsigned_elt) kind

type c_layout

type fortran_layout

type 'a layout

val c_layout : c_layout layout
val fortran_layout : fortran_layout layout


module Genarray :
  sig
  type ('a, 'b, 'c) t

  external create: ('a, 'b) kind -> 'c layout -> int array -> ('a, 'b, 'c) t
    = "caml_ba_create"

  external num_dims: ('a, 'b, 'c) t -> int = "caml_ba_num_dims"

  val dims : ('a, 'b, 'c) t -> int array

  external nth_dim: ('a, 'b, 'c) t -> int -> int = "caml_ba_dim"

  external kind: ('a, 'b, 'c) t -> ('a, 'b) kind = "caml_ba_kind"

  external layout: ('a, 'b, 'c) t -> 'c layout = "caml_ba_layout"

  external get: ('a, 'b, 'c) t -> int array -> 'a = "caml_ba_get_generic"

  external set: ('a, 'b, 'c) t -> int array -> 'a -> unit
    = "caml_ba_set_generic"

  external sub_left: ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) t
    = "caml_ba_sub"

  external sub_right:
    ('a, 'b, fortran_layout) t -> int -> int -> ('a, 'b, fortran_layout) t
    = "caml_ba_sub"

  external slice_left:
    ('a, 'b, c_layout) t -> int array -> ('a, 'b, c_layout) t
    = "caml_ba_slice"

  external slice_right:
    ('a, 'b, fortran_layout) t -> int array -> ('a, 'b, fortran_layout) t
    = "caml_ba_slice"

  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit
      = "caml_ba_blit"

  external fill: ('a, 'b, 'c) t -> 'a -> unit = "caml_ba_fill"

  end

module Array1 : sig
  type ('a, 'b, 'c) t

  val create: ('a, 'b) kind -> 'c layout -> int -> ('a, 'b, 'c) t

  external dim: ('a, 'b, 'c) t -> int = "%caml_ba_dim_1"

  external kind: ('a, 'b, 'c) t -> ('a, 'b) kind = "caml_ba_kind"

  external layout: ('a, 'b, 'c) t -> 'c layout = "caml_ba_layout"

  external get: ('a, 'b, 'c) t -> int -> 'a = "%caml_ba_ref_1"

  external set: ('a, 'b, 'c) t -> int -> 'a -> unit = "%caml_ba_set_1"

  external sub: ('a, 'b, 'c) t -> int -> int -> ('a, 'b, 'c) t
      = "caml_ba_sub"

  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit
      = "caml_ba_blit"

  external fill: ('a, 'b, 'c) t -> 'a -> unit = "caml_ba_fill"

  val of_array: ('a, 'b) kind -> 'c layout -> 'a array -> ('a, 'b, 'c) t

  external unsafe_get: ('a, 'b, 'c) t -> int -> 'a = "%caml_ba_unsafe_ref_1"
  external unsafe_set: ('a, 'b, 'c) t -> int -> 'a -> unit
                     = "%caml_ba_unsafe_set_1"

end

external genarray_of_array1 :
  ('a, 'b, 'c) Array1.t -> ('a, 'b, 'c) Genarray.t = "%identity"

val array1_of_genarray : ('a, 'b, 'c) Genarray.t -> ('a, 'b, 'c) Array1.t
val reshape : ('a, 'b, 'c) Genarray.t -> int array -> ('a, 'b, 'c) Genarray.t

val reshape_1 : ('a, 'b, 'c) Genarray.t -> int -> ('a, 'b, 'c) Array1.t

