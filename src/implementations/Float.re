open Interface;

/* Note: `float` is not a fully law abiding member of Additive.Semigroup,
 * Multiplicative.Semigroup, Semiring, Division_Ring, and Euclidean_Ring, and any
 * abstractions dependent on these, due to potential arithmetic overflows and
 * floating point precision issues
 */

let approximately_equal: (~tolerance: float, float, float) => bool =
  (~tolerance, a, b) => Js.Math.abs_float(a -. b) <= tolerance;

module Additive = {
  module Magma: MAGMA with type t = float = {
    type t = float;
    let append = (+.);
  };
  module Medial_Magma: MEDIAL_MAGMA with type t = float = Magma;
  module Semigroup: SEMIGROUP with type t = float = {
    include Magma;
  };
  module Monoid: MONOID with type t = float = {
    include Semigroup;
    let empty = 0.0;
  };
  module Quasigroup: QUASIGROUP with type t = float = {
    include Magma;
  };
  module Medial_Quasigroup: MEDIAL_QUASIGROUP with type t = float = {
    include Medial_Magma;
  };
  module Loop: LOOP with type t = float = {
    include Quasigroup;
    let empty = 0.0;
  };
  module Group: GROUP with type t = float = {
    include Monoid;
    let inverse = ( *. )(-1.0);
  };
  module Abelian_Group: ABELIAN_GROUP with type t = float = {
    include Group;
  };
};

module Multiplicative = {
  module Magma: MAGMA with type t = float = {
    type t = float;
    let append = ( *. );
  };
  module Medial_Magma: MEDIAL_MAGMA with type t = float = Magma;
  module Semigroup: SEMIGROUP with type t = float = {
    include Magma;
  };
  module Monoid: MONOID with type t = float = {
    include Semigroup;
    let empty = 1.0;
  };
  module Quasigroup: QUASIGROUP with type t = float = {
    include Magma;
  };
  module Medial_Quasigroup: MEDIAL_QUASIGROUP with type t = float = {
    include Medial_Magma;
  };
  module Loop: LOOP with type t = float = {
    include Quasigroup;
    let empty = 1.0;
  };
};

module Subtractive = {
  module Magma: MAGMA with type t = float = {
    type t = float;
    let append = (-.);
  };
  module Medial_Magma: MEDIAL_MAGMA with type t = float = Magma;
  module Quasigroup: QUASIGROUP with type t = float = {
    include Magma;
  };
  module Medial_Quasigroup: MEDIAL_QUASIGROUP with type t = float = {
    include Medial_Magma;
  };
};

module Divisive = {
  module Magma: MAGMA with type t = float = {
    type t = float;
    let append = (/.);
  };
  module Medial_Magma: MEDIAL_MAGMA with type t = float = Magma;
  module Quasigroup: QUASIGROUP with type t = float = {
    include Magma;
  };
  module Medial_Quasigroup: MEDIAL_QUASIGROUP with type t = float = {
    include Medial_Magma;
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
  [@bs.val] external max_value : float = "Number.MAX_VALUE";
  [@bs.val] external min_value : float = "Number.MIN_VALUE";

  let top = max_value
  and bottom = min_value;
};

module Show: SHOW with type t = float = {
  type t = float;
  let show = Js.Float.toString;
};

module Semiring: SEMIRING with type t = float = {
  type t = float;
  let add = (+.);
  let zero = 0.0;
  let multiply = ( *. );
  let one = 1.0;
};

module Ring: RING with type t = float = {
  include Semiring;
  let subtract = (-.);
};

module Commutative_Ring: COMMUTATIVE_RING with type t = float = {
  include Ring;
};

module Division_Ring: DIVISION_RING with type t = float = {
  include Ring;
  let reciprocal = a => 1.0 /. a;
};

module Euclidean_Ring: EUCLIDEAN_RING with type t = float = {
  include Commutative_Ring;
  let degree = _ => 1;
  let divide = (/.);
  let modulo = (_, _) => 0.0;
};

module Field: FIELD with type t = float = {
  include Euclidean_Ring;
  include (Division_Ring: DIVISION_RING with type t := t);
};

module Infix = {
  module Additive = {
    include Infix.Magma(Additive.Magma);
  };
  module Multiplicative = {
    include Infix.Magma(Multiplicative.Magma);
  };
  include Infix.Eq(Eq);
  include Infix.Ord(Ord);
  include Infix.Euclidean_Ring(Euclidean_Ring);
};
