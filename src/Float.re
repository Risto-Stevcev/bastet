open Interface;

let approximately_equal: (~tolerance:float, float, float) => bool =
  (~tolerance, a, b) => Js.Math.abs_float(a -. b) <= tolerance;

module Additive = {
  module Semigroup: SEMIGROUP with type t = float = {
    type t = float;
    let append = (+.);
  };
  module Monoid: MONOID with type t = float = {
    include Semigroup;
    let empty = 0.0;
  };
};

module Multiplicative = {
  module Semigroup: SEMIGROUP with type t = float = {
    type t = float;
    let append = (*.);
  };
  module Monoid: MONOID with type t = float = {
    include Semigroup;
    let empty = 1.0;
  };
};

module Eq: EQ with type t = float = {
  type t = float;
  let eq = (==);
};

module Ord: ORD with type t = float = {
  include Eq;
  let compare = unsafe_compare;
};

module Bounded: BOUNDED with type t = float = {
  include Ord;
  let top = 2147483647.0;
  let bottom = -2147483648.0;
};

module Show: SHOW with type t = float = {
  type t = float;
  let show = string_of_float;
};

module Semiring: SEMIRING with type t = float = {
  type t = float;
  let add = (+.);
  let zero = 0.0;
  let multiply = (*.);
  let one = 1.0;
};

module Infix = {
  module Additive = {
    module Semigroup = Infix.Semigroup(Additive.Semigroup);
  };
  module Multiplicative = {
    module Semigroup = Infix.Semigroup(Multiplicative.Semigroup);
  };
  module Eq = Infix.Eq(Eq);
  module Ord = Infix.Ord(Ord);
  module Semiring = Infix.Semiring(Semiring);
};
