
module Fft = Fftw3.D
open Fft

type direction = [`Forward | `Backward]
type work = [`ColumnWise | `RowWise]

type ('a, 'b, 'c) t = {
  plan:  'a Fft.plan;
  input: 'b;
  output: 'c;
  norm : bool;
}

type rvec = R.vec
type cvec = C.vec
type rmat = R.mat
type cmat = C.mat

let rkind,rlayout = R.Scalar.(kind, layout)
let ckind,clayout = C.Scalar.(kind, layout)

let solve t =
  exec t.plan;
  t.output

module Vec = struct
  type cc = (c2c,cvec,cvec) t
  type rc = (r2c,rvec,cvec) t
  type cr = (c2r,cvec,rvec) t

  let c2c_solver n dir =
    let dir,norm = match dir with
      | `Forward -> Forward,false
      | `Backward -> Backward,true in
    let input = Array1.create ckind clayout n in
    let output = Array1.create ckind clayout n in
    let plan = Array1.dft dir input output in
    {plan; input; output; norm;}

  let r2c_solver n =
    let input = Array1.create rkind rlayout n in
    let output = Array1.create ckind clayout (n / 2 + 1) in
    let plan = Array1.r2c input output in
    {plan; input; output; norm=false}

  let c2r_solver n =
    let input = Array1.create ckind clayout (n / 2 + 1) in
    let output = Array1.create rkind rlayout n in
    let plan = Array1.c2r input output in
    {plan; input; output; norm=true}

  let solve_c2c v t =
    C.Vector.blit v t.input;
    if t.norm then
      let d = float_of_int (C.Vector.dim v) in
      let c = Complex.({re = 1.0 /. d; im = 0.0}) in
      let v' = solve t in
      let () = C.scal c v' in
      v'
    else
      solve t

  let solve_r2c v t =
    R.Vector.blit v t.input;
    solve t

  let solve_c2r v t =
    C.Vector.blit v t.input;
    let v' = solve t in
    let c = 1.0 /. float_of_int (R.Vector.dim v') in
    let () = R.scal c v' in
    v'
end

module Mat = struct
  type cc = (c2c,cmat,cmat) t * work
  type rc = (r2c,rmat,cmat) t * work
  type cr = (c2r,cmat,rmat) t * work

  (** returns inci,inco,hownanyi,howmanyo correspondently *)
  let increments = function
    | `ColumnWise -> (1,0), (1,0), [1,1;0,1], [1,1;0,1]
    | `RowWise -> (0,1), (0,1), [1,0;], [1,0]

  let c2c_solver ?(work=`ColumnWise) m n dir =
    let dir,norm = match dir with
      | `Forward -> Fft.Forward,false
      | `Backward -> Fft.Backward,true in
    let input = Array2.create ckind clayout m n in
    let output = Array2.create ckind clayout m n in
    let inci,inco,howmanyi,howmanyo = increments work in
    let plan = Array2.dft dir ~inci ~inco ~howmanyi
        ~howmanyo input output in
    {plan; input; output;norm}, work

  let r2c_solver ?(work=`ColumnWise) m n =
    let d1, d2 = match work with
      | `ColumnWise -> (m / 2 + 1), n
      | `RowWise -> m, (n / 2 + 1) in
    let input = Array2.create rkind rlayout m n in
    let output = Array2.create ckind clayout d1 d2 in
    let inci,inco,howmanyi,howmanyo = increments work in
    let plan = Array2.r2c ~inci ~inco ~howmanyi
        ~howmanyo input output in
    {plan; input; output; norm=false}, work

  let c2r_solver ?(work=`ColumnWise) m n =
    let d1, d2 = match work with
      | `ColumnWise -> (m / 2 + 1), n
      | `RowWise -> m, (n / 2 + 1) in
    let input = Array2.create ckind clayout d1 d2 in
    let output = Array2.create rkind rlayout m n in
    let inci,inco,howmanyi,howmanyo = increments work in
    let plan = Array2.c2r ~inci ~inco ~howmanyi
        ~howmanyo input output in
    {plan; input; output; norm=true}, work

  let solve_c2c mat (t,w) =
    C.Matrix.blit mat t.input;
    let mat' = solve t in
    if t.norm then
      let dim = match w with
        | `ColumnWise -> C.Matrix.dim1 mat
        | `RowWise -> C.Matrix.dim2 mat in
      let c = Complex.({re = 1.0 /. float_of_int dim; im = 0.0}) in
      let () = C.Matrix.scal c mat' in
      mat'
    else
      mat'

  let solve_r2c mat (t,_) =
    R.Matrix.blit mat t.input;
    solve t

  let solve_c2r mat (t,w) =
    C.Matrix.blit mat t.input;
    let mat' = solve t in
    let dim = match w with
      | `ColumnWise -> R.Matrix.dim1 mat'
      | `RowWise -> R.Matrix.dim2 mat' in
    let c = 1.0 /. float_of_int dim in
    let () = R.Matrix.scal c mat' in
    mat'

end
