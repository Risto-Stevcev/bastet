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
