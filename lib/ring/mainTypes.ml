
(** Module for uniform using functions from Bigarray and Lacaml
    modules. *)

open Basic
open BaseArray

(** Basic module for vectors.
    Module includes Array1 and some special cases.
    For example, function sub is overriden in Lacaml
    for substituion. *)
module CommonVector = struct
  include Array1
  let part = sub
end

(** module for real numbers *)
module Real = struct

  include Lacaml.D

  module Scalar = ScalarR

  module Vector = struct
    include CommonVector
    include Vec
  end

  module Matrix = struct
    include Array2
    include Mat
  end

end

(** module for complex numbers *)
module Complex = struct
  include Lacaml.Z

  module Scalar = ScalarZ

  module Vector = struct
    include CommonVector
    include Vec
  end

  module Matrix = struct
    include Array2
    include Mat
  end

end

(** module for int numbers.
    Note, that Int module has completly different interface
    because int arrays aren't maintained by lacaml, hence it lacks
    lots of useful functions *)
module Int = struct

  module Scalar = ScalarI

  module Vector = struct
    include CommonVector
    type v = (ScalarI.num_type, ScalarI.elt, ScalarI.layout) t
    let make = Array1.create int ScalarI.layout
  end

  module Matrix = struct
    include Array2
    type m = (ScalarI.num_type, ScalarI.elt, ScalarI.layout) t
    let make = Array2.create int ScalarI.layout
  end

  type vec = Vector.v
  type mat = Matrix.m

end

