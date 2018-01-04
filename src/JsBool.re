open Interface;
open Js.Boolean;
let (<.) = Function.Infix.(<.);
let (to_bool, true_, false_) = Js.(to_bool, true_, false_);

module Conjunctive = {
  module Magma: MAGMA with type t = Js.boolean = {
    type t = Js.boolean;
    let append = (a, b) => to_js_boolean(to_bool(a) && to_bool(b));
  };
  module Medial_Magma: MEDIAL_MAGMA with type t = Js.boolean = Magma;
  module Semigroup: SEMIGROUP with type t = Js.boolean = {
    include Magma;
  };
  module Monoid: MONOID with type t = Js.boolean = {
    include Semigroup;
    let empty = true_;
  };
};

module Disjunctive = {
  module Magma: MAGMA with type t = Js.boolean = {
    type t = Js.boolean;
    let append = (a, b) => to_js_boolean(to_bool(a) || to_bool(b));
  };
  module Medial_Magma: MEDIAL_MAGMA with type t = Js.boolean = Magma;
  module Semigroup: SEMIGROUP with type t = Js.boolean = {
    include Magma;
  };
  module Monoid: MONOID with type t = Js.boolean = {
    include Semigroup;
    let empty = false_;
  };
};

module Eq: EQ with type t = Js.boolean = {
  type t = Js.boolean;
  let eq = (==);
};

module Ord: ORD with type t = Js.boolean = {
  include Eq;
  let compare = unsafe_compare;
};

module Bounded: BOUNDED with type t = Js.boolean = {
  include Ord;
  let top = true_;
  let bottom = false_;
};

module Join_Semilattice: JOIN_SEMILATTICE with type t = Js.boolean = {
  type t = Js.boolean;
  let join = (a, b) => to_js_boolean(to_bool(a) || to_bool(b));
};

module Meet_Semilattice: MEET_SEMILATTICE with type t = Js.boolean = {
  type t = Js.boolean;
  let meet = (a, b) => to_js_boolean(to_bool(a) && to_bool(b));
};

module Bounded_Join_Semilattice: BOUNDED_JOIN_SEMILATTICE with type t = Js.boolean = {
  include Join_Semilattice;
  let bottom = false_;
};

module Bounded_Meet_Semilattice: BOUNDED_MEET_SEMILATTICE with type t = Js.boolean = {
  include Meet_Semilattice;
  let top = true_;
};

module Lattice: LATTICE with type t = Js.boolean = {
  include Join_Semilattice;
  include (Meet_Semilattice: MEET_SEMILATTICE with type t := t);
};

module Bounded_Lattice: BOUNDED_LATTICE with type t = Js.boolean = {
  include Bounded_Join_Semilattice;
  include (Bounded_Meet_Semilattice: BOUNDED_MEET_SEMILATTICE with type t := t);
};

module Distributive_Lattice: DISTRIBUTIVE_LATTICE with type t = Js.boolean = {
  include Lattice;
};

module Bounded_Distributive_Lattice: BOUNDED_DISTRIBUTIVE_LATTICE with type t = Js.boolean = {
  include Bounded_Lattice;
};

module Heyting_Algebra: HEYTING_ALGEBRA with type t = Js.boolean = {
  include Ord;
  include (Bounded_Distributive_Lattice: BOUNDED_DISTRIBUTIVE_LATTICE with type t := t);
  let not = (a) => to_js_boolean(!to_bool(a));
  let implies = (a, b) => join(not(a), b);
};

module Involutive_Heyting_Algebra: INVOLUTIVE_HEYTING_ALGEBRA with type t = Js.boolean = {
  include Heyting_Algebra;
};

module Boolean_Algebra: BOOLEAN_ALGEBRA with type t = Js.boolean = {
  include Heyting_Algebra;
};

module Show: SHOW with type t = Js.boolean = {
  type t = Js.boolean;
  let show = string_of_bool <. to_bool;
};

module Infix = {
  module Conjunctive = {
    include Infix.Magma(Conjunctive.Magma)
  };
  module Disjunctive = {
    include Infix.Magma(Disjunctive.Magma)
  };
  include Infix.Eq(Eq);
  include Infix.Ord(Ord);
  include Infix.Join_Semilattice(Join_Semilattice);
  include Infix.Meet_Semilattice(Meet_Semilattice);
  include Infix.Heyting_Algebra(Heyting_Algebra);
};
