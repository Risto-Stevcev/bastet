open Interface;

module Conjunctive = {
  module Semigroup: SEMIGROUP with type t = bool = {
    type t = bool;
    let append = (&&);
  };
  module Monoid: MONOID with type t = bool = {
    include Semigroup;
    let empty = true;
  };
};

module Disjunctive = {
  module Semigroup: SEMIGROUP with type t = bool = {
    type t = bool;
    let append = (||);
  };
  module Monoid: MONOID with type t = bool = {
    include Semigroup;
    let empty = false;
  };
};

module Eq: EQ with type t = bool = {
  type t = bool;
  let eq = (==);
};

module Ord: ORD with type t = bool = {
  include Eq;
  let compare = unsafe_compare;
};

module Bounded: BOUNDED with type t = bool = {
  include Ord;
  let top = true;
  let bottom = false;
};

module Join_Semilattice: JOIN_SEMILATTICE with type t = bool = {
  type t = bool;
  let join = (||);
};

module Meet_Semilattice: MEET_SEMILATTICE with type t = bool = {
  type t = bool;
  let meet = (&&);
};

module Bounded_Join_Semilattice: BOUNDED_JOIN_SEMILATTICE with type t = bool = {
  include Join_Semilattice;
  let bottom = false;
};

module Bounded_Meet_Semilattice: BOUNDED_MEET_SEMILATTICE with type t = bool = {
  include Meet_Semilattice;
  let top = true;
};

module Lattice: LATTICE with type t = bool = {
  include Join_Semilattice;
  include (Meet_Semilattice: MEET_SEMILATTICE with type t := t);
};

module Bounded_Lattice: BOUNDED_LATTICE with type t = bool = {
  include Bounded_Join_Semilattice;
  include (Bounded_Meet_Semilattice: BOUNDED_MEET_SEMILATTICE with type t := t);
};

module Distributive_Lattice: DISTRIBUTIVE_LATTICE with type t = bool = {
  include Lattice;
};

module Bounded_Distributive_Lattice: BOUNDED_DISTRIBUTIVE_LATTICE with type t = bool = {
  include Bounded_Lattice;
};

module Heyting_Algebra: HEYTING_ALGEBRA with type t = bool = {
  include Ord;
  include (Bounded_Distributive_Lattice: BOUNDED_DISTRIBUTIVE_LATTICE with type t := t);
  let not = (a) => !a;
  let implies = (a, b) => !a || b;
};

module Involutive_Heyting_Algebra: INVOLUTIVE_HEYTING_ALGEBRA with type t = bool = {
  include Heyting_Algebra;
};

module Boolean_Algebra: BOOLEAN_ALGEBRA with type t = bool = {
  include Heyting_Algebra;
};

module Show: SHOW with type t = bool = {
  type t = bool;
  let show = string_of_bool;
};
