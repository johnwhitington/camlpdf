(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Manuel Serrano et Xavier Leroy, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* Module [Bigarray]: large, multi-dimensional, numerical arrays *)

external init : unit -> unit = "caml_ba_init"

let _ = init()

type ('a, 'b) kind = int

type int8_unsigned_elt

(* Keep those constants in sync with the caml_ba_kind enumeration
   in bigarray.h *)

let int8_unsigned = 3

type 'a layout = int

type c_layout
type fortran_layout

(* Keep those constants in sync with the caml_ba_layout enumeration
   in bigarray.h *)

let c_layout = 0
let fortran_layout = 0x100

module Genarray = struct
  type ('a, 'b, 'c) t
  external create: ('a, 'b) kind -> 'c layout -> int array -> ('a, 'b, 'c) t
     = "caml_ba_create"
  external get: ('a, 'b, 'c) t -> int array -> 'a
     = "caml_ba_get_generic"
  external set: ('a, 'b, 'c) t -> int array -> 'a -> unit
     = "caml_ba_set_generic"
  external num_dims: ('a, 'b, 'c) t -> int = "caml_ba_num_dims"
  external nth_dim: ('a, 'b, 'c) t -> int -> int = "caml_ba_dim"
  let dims a =
    let n = num_dims a in
    let d = Array.make n 0 in
    for i = 0 to n-1 do d.(i) <- nth_dim a i done;
    d
  external kind: ('a, 'b, 'c) t -> ('a, 'b) kind = "caml_ba_kind"
  external layout: ('a, 'b, 'c) t -> 'c layout = "caml_ba_layout"

  external sub_left: ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) t
     = "caml_ba_sub"
  external sub_right: ('a, 'b, fortran_layout) t -> int -> int ->
                          ('a, 'b, fortran_layout) t
     = "caml_ba_sub"
  external slice_left: ('a, 'b, c_layout) t -> int array ->
                          ('a, 'b, c_layout) t
     = "caml_ba_slice"
  external slice_right: ('a, 'b, fortran_layout) t -> int array ->
                          ('a, 'b, fortran_layout) t
     = "caml_ba_slice"
  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit
     = "caml_ba_blit"
  external fill: ('a, 'b, 'c) t -> 'a -> unit = "caml_ba_fill"
end

module Array1 = struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) Genarray.t
  let create kind layout dim =
    Genarray.create kind layout [|dim|]
  external get: ('a, 'b, 'c) t -> int -> 'a = "%caml_ba_ref_1"
  external set: ('a, 'b, 'c) t -> int -> 'a -> unit = "%caml_ba_set_1"
  external unsafe_get: ('a, 'b, 'c) t -> int -> 'a = "%caml_ba_unsafe_ref_1"
  external unsafe_set: ('a, 'b, 'c) t -> int -> 'a -> unit
     = "%caml_ba_unsafe_set_1"
  external dim: ('a, 'b, 'c) t -> int = "%caml_ba_dim_1"
  external kind: ('a, 'b, 'c) t -> ('a, 'b) kind = "caml_ba_kind"
  external layout: ('a, 'b, 'c) t -> 'c layout = "caml_ba_layout"
  external sub: ('a, 'b, 'c) t -> int -> int -> ('a, 'b, 'c) t = "caml_ba_sub"
  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit = "caml_ba_blit"
  external fill: ('a, 'b, 'c) t -> 'a -> unit = "caml_ba_fill"
  let of_array kind layout data =
    let ba = create kind layout (Array.length data) in
    let ofs = if layout = c_layout then 0 else 1 in
    for i = 0 to Array.length data - 1 do unsafe_set ba (i + ofs) data.(i) done;
    ba
end

external genarray_of_array1: ('a, 'b, 'c) Array1.t -> ('a, 'b, 'c) Genarray.t
   = "%identity"
let array1_of_genarray a =
  if Genarray.num_dims a = 1 then a
  else invalid_arg "Bigarray.array1_of_genarray"

external reshape:
   ('a, 'b, 'c) Genarray.t -> int array -> ('a, 'b, 'c) Genarray.t
   = "caml_ba_reshape"
let reshape_1 a dim1 = reshape a [|dim1|]

(* Force caml_ba_get_{1,2,3,N} to be linked in, since we don't refer
   to those primitives directly in this file *)

let _ =
  let _ = Genarray.get in
  let _ = Array1.get in
  ()

external get1: unit -> unit = "caml_ba_get_1"
external get2: unit -> unit = "caml_ba_get_2"
external get3: unit -> unit = "caml_ba_get_3"
external set1: unit -> unit = "caml_ba_set_1"
external set2: unit -> unit = "caml_ba_set_2"
external set3: unit -> unit = "caml_ba_set_3"
