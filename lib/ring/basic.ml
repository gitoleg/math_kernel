
(** Special module for basic types. Created to shadow Bigarray
    internal data representaion.

    REMEMBER! numeration of fortran arrays begins from 1 !!! *)

module BaseArray = Bigarray
open BaseArray

module L = struct
  type layout = fortran_layout
  let layout = fortran_layout
end

module type S = sig
  include module type of L
  type num_type
  type elt
  val kind: (num_type, elt) kind
end

module ScalarR = struct
  include L
  type num_type = float
  type elt = float64_elt
  let kind = float64
end

module ScalarZ = struct
  include L
  type num_type = Complex.t
  type elt = complex64_elt
  let kind = complex64
end

module ScalarI = struct
  include L
  type num_type = int
  type elt = int_elt
  let kind = int
end
