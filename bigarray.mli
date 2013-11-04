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

type int8_unsigned_elt

type ('a, 'b) kind
(** To each element kind is associated an OCaml type, which is
   the type of OCaml values that can be stored in the big array
   or read back from it.  This type is not necessarily the same
   as the type of the array elements proper: for instance,
   a big array whose elements are of kind [float32_elt] contains
   32-bit single precision floats, but reading or writing one of
   its elements from OCaml uses the OCaml type [float], which is
   64-bit double precision floats.

   The abstract type [('a, 'b) kind] captures this association
   of an OCaml type ['a] for values read or written in the big array,
   and of an element kind ['b] which represents the actual contents
   of the big array.  The following predefined values of type
   [kind] list all possible associations of OCaml types with
   element kinds: *)

val int8_unsigned : (int, int8_unsigned_elt) kind

(** {6 Array layouts} *)

type c_layout
(** See {!Bigarray.fortran_layout}.*)

type fortran_layout
(** To facilitate interoperability with existing C and Fortran code,
   this library supports two different memory layouts for big arrays,
   one compatible with the C conventions,
   the other compatible with the Fortran conventions.

   In the C-style layout, array indices start at 0, and
   multi-dimensional arrays are laid out in row-major format.
   That is, for a two-dimensional array, all elements of
   row 0 are contiguous in memory, followed by all elements of
   row 1, etc.  In other terms, the array elements at [(x,y)]
   and [(x, y+1)] are adjacent in memory.

   In the Fortran-style layout, array indices start at 1, and
   multi-dimensional arrays are laid out in column-major format.
   That is, for a two-dimensional array, all elements of
   column 0 are contiguous in memory, followed by all elements of
   column 1, etc.  In other terms, the array elements at [(x,y)]
   and [(x+1, y)] are adjacent in memory.

   Each layout style is identified at the type level by the
   abstract types {!Bigarray.c_layout} and [fortran_layout] respectively. *)

type 'a layout
(** The type ['a layout] represents one of the two supported
   memory layouts: C-style if ['a] is {!Bigarray.c_layout}, Fortran-style
   if ['a] is {!Bigarray.fortran_layout}. *)


(** {7 Supported layouts}

   The abstract values [c_layout] and [fortran_layout] represent
   the two supported layouts at the level of values.
*)

val c_layout : c_layout layout
val fortran_layout : fortran_layout layout


(** {6 Generic arrays (of arbitrarily many dimensions)} *)

module Genarray :
  sig
  type ('a, 'b, 'c) t
  (** The type [Genarray.t] is the type of big arrays with variable
     numbers of dimensions.  Any number of dimensions between 1 and 16
     is supported.

     The three type parameters to [Genarray.t] identify the array element
     kind and layout, as follows:
     - the first parameter, ['a], is the OCaml type for accessing array
       elements ([float], [int], [int32], [int64], [nativeint]);
     - the second parameter, ['b], is the actual kind of array elements
       ([float32_elt], [float64_elt], [int8_signed_elt], [int8_unsigned_elt],
       etc);
     - the third parameter, ['c], identifies the array layout
       ([c_layout] or [fortran_layout]).

     For instance, [(float, float32_elt, fortran_layout) Genarray.t]
     is the type of generic big arrays containing 32-bit floats
     in Fortran layout; reads and writes in this array use the
     OCaml type [float]. *)

  external create: ('a, 'b) kind -> 'c layout -> int array -> ('a, 'b, 'c) t
    = "caml_ba_create"
  (** [Genarray.create kind layout dimensions] returns a new big array
     whose element kind is determined by the parameter [kind] (one of
     [float32], [float64], [int8_signed], etc) and whose layout is
     determined by the parameter [layout] (one of [c_layout] or
     [fortran_layout]).  The [dimensions] parameter is an array of
     integers that indicate the size of the big array in each dimension.
     The length of [dimensions] determines the number of dimensions
     of the bigarray.

     For instance, [Genarray.create int32 c_layout [|4;6;8|]]
     returns a fresh big array of 32-bit integers, in C layout,
     having three dimensions, the three dimensions being 4, 6 and 8
     respectively.

     Big arrays returned by [Genarray.create] are not initialized:
     the initial values of array elements is unspecified.

     [Genarray.create] raises [Invalid_argument] if the number of dimensions
     is not in the range 1 to 16 inclusive, or if one of the dimensions
     is negative. *)

  external num_dims: ('a, 'b, 'c) t -> int = "caml_ba_num_dims"
  (** Return the number of dimensions of the given big array. *)

  val dims : ('a, 'b, 'c) t -> int array
  (** [Genarray.dims a] returns all dimensions of the big array [a],
     as an array of integers of length [Genarray.num_dims a]. *)

  external nth_dim: ('a, 'b, 'c) t -> int -> int = "caml_ba_dim"
  (** [Genarray.nth_dim a n] returns the [n]-th dimension of the
     big array [a].  The first dimension corresponds to [n = 0];
     the second dimension corresponds to [n = 1]; the last dimension,
     to [n = Genarray.num_dims a - 1].
     Raise [Invalid_argument] if [n] is less than 0 or greater or equal than
     [Genarray.num_dims a]. *)

  external kind: ('a, 'b, 'c) t -> ('a, 'b) kind = "caml_ba_kind"
  (** Return the kind of the given big array. *)

  external layout: ('a, 'b, 'c) t -> 'c layout = "caml_ba_layout"
  (** Return the layout of the given big array. *)

  external get: ('a, 'b, 'c) t -> int array -> 'a = "caml_ba_get_generic"
  (** Read an element of a generic big array.
     [Genarray.get a [|i1; ...; iN|]] returns the element of [a]
     whose coordinates are [i1] in the first dimension, [i2] in
     the second dimension, ..., [iN] in the [N]-th dimension.

     If [a] has C layout, the coordinates must be greater or equal than 0
     and strictly less than the corresponding dimensions of [a].
     If [a] has Fortran layout, the coordinates must be greater or equal
     than 1 and less or equal than the corresponding dimensions of [a].
     Raise [Invalid_argument] if the array [a] does not have exactly [N]
     dimensions, or if the coordinates are outside the array bounds.

     If [N > 3], alternate syntax is provided: you can write
     [a.{i1, i2, ..., iN}] instead of [Genarray.get a [|i1; ...; iN|]].
     (The syntax [a.{...}] with one, two or three coordinates is
     reserved for accessing one-, two- and three-dimensional arrays
     as described below.) *)

  external set: ('a, 'b, 'c) t -> int array -> 'a -> unit
    = "caml_ba_set_generic"
  (** Assign an element of a generic big array.
     [Genarray.set a [|i1; ...; iN|] v] stores the value [v] in the
     element of [a] whose coordinates are [i1] in the first dimension,
     [i2] in the second dimension, ..., [iN] in the [N]-th dimension.

     The array [a] must have exactly [N] dimensions, and all coordinates
     must lie inside the array bounds, as described for [Genarray.get];
     otherwise, [Invalid_argument] is raised.

     If [N > 3], alternate syntax is provided: you can write
     [a.{i1, i2, ..., iN} <- v] instead of
     [Genarray.set a [|i1; ...; iN|] v].
     (The syntax [a.{...} <- v] with one, two or three coordinates is
     reserved for updating one-, two- and three-dimensional arrays
     as described below.) *)

  external sub_left: ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) t
    = "caml_ba_sub"
  (** Extract a sub-array of the given big array by restricting the
     first (left-most) dimension.  [Genarray.sub_left a ofs len]
     returns a big array with the same number of dimensions as [a],
     and the same dimensions as [a], except the first dimension,
     which corresponds to the interval [[ofs ... ofs + len - 1]]
     of the first dimension of [a].  No copying of elements is
     involved: the sub-array and the original array share the same
     storage space.  In other terms, the element at coordinates
     [[|i1; ...; iN|]] of the sub-array is identical to the
     element at coordinates [[|i1+ofs; ...; iN|]] of the original
     array [a].

     [Genarray.sub_left] applies only to big arrays in C layout.
     Raise [Invalid_argument] if [ofs] and [len] do not designate
     a valid sub-array of [a], that is, if [ofs < 0], or [len < 0],
     or [ofs + len > Genarray.nth_dim a 0]. *)

  external sub_right:
    ('a, 'b, fortran_layout) t -> int -> int -> ('a, 'b, fortran_layout) t
    = "caml_ba_sub"
  (** Extract a sub-array of the given big array by restricting the
     last (right-most) dimension.  [Genarray.sub_right a ofs len]
     returns a big array with the same number of dimensions as [a],
     and the same dimensions as [a], except the last dimension,
     which corresponds to the interval [[ofs ... ofs + len - 1]]
     of the last dimension of [a].  No copying of elements is
     involved: the sub-array and the original array share the same
     storage space.  In other terms, the element at coordinates
     [[|i1; ...; iN|]] of the sub-array is identical to the
     element at coordinates [[|i1; ...; iN+ofs|]] of the original
     array [a].

     [Genarray.sub_right] applies only to big arrays in Fortran layout.
     Raise [Invalid_argument] if [ofs] and [len] do not designate
     a valid sub-array of [a], that is, if [ofs < 1], or [len < 0],
     or [ofs + len > Genarray.nth_dim a (Genarray.num_dims a - 1)]. *)

  external slice_left:
    ('a, 'b, c_layout) t -> int array -> ('a, 'b, c_layout) t
    = "caml_ba_slice"
  (** Extract a sub-array of lower dimension from the given big array
     by fixing one or several of the first (left-most) coordinates.
     [Genarray.slice_left a [|i1; ... ; iM|]] returns the 'slice'
     of [a] obtained by setting the first [M] coordinates to
     [i1], ..., [iM].  If [a] has [N] dimensions, the slice has
     dimension [N - M], and the element at coordinates
     [[|j1; ...; j(N-M)|]] in the slice is identical to the element
     at coordinates [[|i1; ...; iM; j1; ...; j(N-M)|]] in the original
     array [a].  No copying of elements is involved: the slice and
     the original array share the same storage space.

     [Genarray.slice_left] applies only to big arrays in C layout.
     Raise [Invalid_argument] if [M >= N], or if [[|i1; ... ; iM|]]
     is outside the bounds of [a]. *)

  external slice_right:
    ('a, 'b, fortran_layout) t -> int array -> ('a, 'b, fortran_layout) t
    = "caml_ba_slice"
  (** Extract a sub-array of lower dimension from the given big array
     by fixing one or several of the last (right-most) coordinates.
     [Genarray.slice_right a [|i1; ... ; iM|]] returns the 'slice'
     of [a] obtained by setting the last [M] coordinates to
     [i1], ..., [iM].  If [a] has [N] dimensions, the slice has
     dimension [N - M], and the element at coordinates
     [[|j1; ...; j(N-M)|]] in the slice is identical to the element
     at coordinates [[|j1; ...; j(N-M); i1; ...; iM|]] in the original
     array [a].  No copying of elements is involved: the slice and
     the original array share the same storage space.

     [Genarray.slice_right] applies only to big arrays in Fortran layout.
     Raise [Invalid_argument] if [M >= N], or if [[|i1; ... ; iM|]]
     is outside the bounds of [a]. *)

  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit
      = "caml_ba_blit"
  (** Copy all elements of a big array in another big array.
     [Genarray.blit src dst] copies all elements of [src] into
     [dst].  Both arrays [src] and [dst] must have the same number of
     dimensions and equal dimensions.  Copying a sub-array of [src]
     to a sub-array of [dst] can be achieved by applying [Genarray.blit]
     to sub-array or slices of [src] and [dst]. *)

  external fill: ('a, 'b, 'c) t -> 'a -> unit = "caml_ba_fill"
  (** Set all elements of a big array to a given value.
     [Genarray.fill a v] stores the value [v] in all elements of
     the big array [a].  Setting only some elements of [a] to [v]
     can be achieved by applying [Genarray.fill] to a sub-array
     or a slice of [a]. *)

  end

(** {6 One-dimensional arrays} *)

(** One-dimensional arrays. The [Array1] structure provides operations
   similar to those of
   {!Bigarray.Genarray}, but specialized to the case of one-dimensional arrays.
   (The [Array2] and [Array3] structures below provide operations
   specialized for two- and three-dimensional arrays.)
   Statically knowing the number of dimensions of the array allows
   faster operations, and more precise static type-checking. *)
module Array1 : sig
  type ('a, 'b, 'c) t
  (** The type of one-dimensional big arrays whose elements have
     OCaml type ['a], representation kind ['b], and memory layout ['c]. *)

  val create: ('a, 'b) kind -> 'c layout -> int -> ('a, 'b, 'c) t
  (** [Array1.create kind layout dim] returns a new bigarray of
     one dimension, whose size is [dim].  [kind] and [layout]
     determine the array element kind and the array layout
     as described for [Genarray.create]. *)

  external dim: ('a, 'b, 'c) t -> int = "%caml_ba_dim_1"
  (** Return the size (dimension) of the given one-dimensional
     big array. *)

  external kind: ('a, 'b, 'c) t -> ('a, 'b) kind = "caml_ba_kind"
  (** Return the kind of the given big array. *)

  external layout: ('a, 'b, 'c) t -> 'c layout = "caml_ba_layout"
  (** Return the layout of the given big array. *)

  external get: ('a, 'b, 'c) t -> int -> 'a = "%caml_ba_ref_1"
  (** [Array1.get a x], or alternatively [a.{x}],
     returns the element of [a] at index [x].
     [x] must be greater or equal than [0] and strictly less than
     [Array1.dim a] if [a] has C layout.  If [a] has Fortran layout,
     [x] must be greater or equal than [1] and less or equal than
     [Array1.dim a].  Otherwise, [Invalid_argument] is raised. *)

  external set: ('a, 'b, 'c) t -> int -> 'a -> unit = "%caml_ba_set_1"
  (** [Array1.set a x v], also written [a.{x} <- v],
     stores the value [v] at index [x] in [a].
     [x] must be inside the bounds of [a] as described in
     {!Bigarray.Array1.get};
     otherwise, [Invalid_argument] is raised. *)

  external sub: ('a, 'b, 'c) t -> int -> int -> ('a, 'b, 'c) t
      = "caml_ba_sub"
  (** Extract a sub-array of the given one-dimensional big array.
     See [Genarray.sub_left] for more details. *)

  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit
      = "caml_ba_blit"
  (** Copy the first big array to the second big array.
     See [Genarray.blit] for more details. *)

  external fill: ('a, 'b, 'c) t -> 'a -> unit = "caml_ba_fill"
  (** Fill the given big array with the given value.
     See [Genarray.fill] for more details. *)

  val of_array: ('a, 'b) kind -> 'c layout -> 'a array -> ('a, 'b, 'c) t
  (** Build a one-dimensional big array initialized from the
     given array.  *)

  external unsafe_get: ('a, 'b, 'c) t -> int -> 'a = "%caml_ba_unsafe_ref_1"
  (** Like {!Bigarray.Array1.get}, but bounds checking is not always performed.
      Use with caution and only when the program logic guarantees that
      the access is within bounds. *)

  external unsafe_set: ('a, 'b, 'c) t -> int -> 'a -> unit
                     = "%caml_ba_unsafe_set_1"
  (** Like {!Bigarray.Array1.set}, but bounds checking is not always performed.
      Use with caution and only when the program logic guarantees that
      the access is within bounds. *)

end

(** {6 Coercions between generic big arrays and fixed-dimension big arrays} *)

external genarray_of_array1 :
  ('a, 'b, 'c) Array1.t -> ('a, 'b, 'c) Genarray.t = "%identity"
(** Return the generic big array corresponding to the given one-dimensional
   big array. *)

val array1_of_genarray : ('a, 'b, 'c) Genarray.t -> ('a, 'b, 'c) Array1.t
(** Return the one-dimensional big array corresponding to the given
   generic big array.  Raise [Invalid_argument] if the generic big array
   does not have exactly one dimension. *)

(** {6 Re-shaping big arrays} *)

val reshape : ('a, 'b, 'c) Genarray.t -> int array -> ('a, 'b, 'c) Genarray.t
(** [reshape b [|d1;...;dN|]] converts the big array [b] to a
   [N]-dimensional array of dimensions [d1]...[dN].  The returned
   array and the original array [b] share their data
   and have the same layout.  For instance, assuming that [b]
   is a one-dimensional array of dimension 12, [reshape b [|3;4|]]
   returns a two-dimensional array [b'] of dimensions 3 and 4.
   If [b] has C layout, the element [(x,y)] of [b'] corresponds
   to the element [x * 3 + y] of [b].  If [b] has Fortran layout,
   the element [(x,y)] of [b'] corresponds to the element
   [x + (y - 1) * 4] of [b].
   The returned big array must have exactly the same number of
   elements as the original big array [b].  That is, the product
   of the dimensions of [b] must be equal to [i1 * ... * iN].
   Otherwise, [Invalid_argument] is raised. *)

val reshape_1 : ('a, 'b, 'c) Genarray.t -> int -> ('a, 'b, 'c) Array1.t
(** Specialized version of {!Bigarray.reshape} for reshaping to
   one-dimensional arrays. *)

