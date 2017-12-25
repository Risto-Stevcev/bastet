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
    let append = ( * );
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
