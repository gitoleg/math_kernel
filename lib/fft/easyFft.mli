
(** fft helpers for more comfortable using fftw3.

    Note, that for c2r_solver functions integer parameters
    expected to be equal to output real array sizes.
*)

type rvec = R.vec
type cvec = C.vec
type rmat = R.mat
type cmat = C.mat

type direction = [`Forward | `Backward]
type work = [`ColumnWise | `RowWise]

module Vec : sig

  type cc
  type rc
  type cr

  val c2c_solver: int -> direction -> cc
  val r2c_solver: int -> rc
  val c2r_solver: int -> cr

  val solve_c2c: cvec -> cc -> cvec
  val solve_r2c: rvec -> rc -> cvec
  val solve_c2r: cvec -> cr -> rvec

end

(** Fft on matrix works column wise by default. *)
module Mat : sig
  type cc
  type rc
  type cr

  (** [c2c_solver ~work d1 d2 direction]
      returns solver for matrix with dimensions d1xd2,
      fft [directtion] and performs transformation
      acording to [work] *)
  val c2c_solver: ?work:work -> int -> int -> direction -> cc
  val r2c_solver: ?work:work -> int -> int -> rc
  val c2r_solver: ?work:work -> int -> int -> cr

  val solve_c2c: cmat -> cc -> cmat
  val solve_r2c: rmat -> rc -> cmat
  val solve_c2r: cmat -> cr -> rmat

end
