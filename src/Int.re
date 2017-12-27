open Interface;

module Additive = {
  module Semigroup: SEMIGROUP with type t = int = {
    type t = int;
    let append = (+);
  };
  module Monoid: MONOID with type t = int = {
    include Semigroup;
    let empty = 0;
  };
};

module Multiplicative = {
  module Semigroup: SEMIGROUP with type t = int = {
    type t = int;
    let append = (*);
  };
  module Monoid: MONOID with type t = int = {
    include Semigroup;
    let empty = 1;
  };
};

module Eq: EQ with type t = int = {
  type t = int;
  let eq = (==);
};

module Ord: ORD with type t = int = {
  include Eq;
  let compare = unsafe_compare;
};

module Bounded: BOUNDED with type t = int = {
  include Ord;
  let top = 2147483647;
  let bottom = -2147483648;
};

module Show: SHOW with type t = int = {
  type t = int;
  let show = string_of_int;
};

module Semiring: SEMIRING with type t = int = {
  type t = int;
  let add = (+);
  let zero = 0;
  let multiply = (*);
  let one = 1;
};

module Ring: RING with type t = int = {
  include Semiring;
  let subtract = (-);
};

module Commutative_Ring: COMMUTATIVE_RING with type t = int = {
  include Ring;
};

module Euclidean_Ring: EUCLIDEAN_RING with type t = int = {
  include Commutative_Ring;
  let degree = (a) => Js.Math.min_int(Js.Math.abs_int(a), Bounded.top);
  let divide = (/);
  let modulo = (a, b) => a mod b;
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
  module Ring = Infix.Ring(Ring);
  module Euclidean_Ring = Infix.Euclidean_Ring(Euclidean_Ring);
};
