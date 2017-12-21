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
