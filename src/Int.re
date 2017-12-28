open Interface;

/* Note: `int` is not a fully law abiding member of Additive.Semigroup,
 * Multiplicative.Semigroup and Semiring, and any abstractions dependent on these,
 * due to potential arithmetic overflows
 */

module Additive = {
  module Magma: MAGMA with type t = int = {
    type t = int;
    let append = (+);
  };
  module Medial_Magma: MEDIAL_MAGMA with type t = int = Magma;
  module Semigroup: SEMIGROUP with type t = int = {
    include Magma;
  };
  module Monoid: MONOID with type t = int = {
    include Semigroup;
    let empty = 0;
  };
  module Quasigroup: QUASIGROUP with type t = int = {
    include Magma;
  };
  module Medial_Quasigroup: MEDIAL_QUASIGROUP with type t = int = {
    include Medial_Magma;
  };
  module Loop: LOOP with type t = int = {
    include Quasigroup;
    let empty = 0;
  };
  module Group: GROUP with type t = int = {
    include Monoid;
    let inverse = (*)(-1);
  };
  module Abelian_Group: ABELIAN_GROUP with type t = int = {
    include Group;
  };
};

module Multiplicative = {
  module Magma: MAGMA with type t = int = {
    type t = int;
    let append = (*);
  };
  module Medial_Magma: MEDIAL_MAGMA with type t = int = Magma;
  module Semigroup: SEMIGROUP with type t = int = {
    include Magma;
  };
  module Monoid: MONOID with type t = int = {
    include Semigroup;
    let empty = 1;
  };
  module Quasigroup: QUASIGROUP with type t = int = {
    include Magma;
  };
  module Loop: LOOP with type t = int = {
    include Quasigroup;
    let empty = 1;
  };
};

module Subtractive = {
  module Magma: MAGMA with type t = int = {
    type t = int;
    let append = (-);
  };
  module Medial_Magma: MEDIAL_MAGMA with type t = int = Magma;
  module Quasigroup: QUASIGROUP with type t = int = {
    include Magma;
  };
};

module Divisive = {
  module Magma: MAGMA with type t = int = {
    type t = int;
    let append = (/);
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
    include Infix.Magma(Additive.Magma);
  };
  module Multiplicative = {
    include Infix.Magma(Multiplicative.Magma);
  };
  include Infix.Eq(Eq);
  include Infix.Ord(Ord);
  include Infix.Euclidean_Ring(Euclidean_Ring);
};
