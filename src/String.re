open Interface;

module Semigroup: SEMIGROUP with type t = string = {
  type t = string;
  let append = (++);
};

module Monoid: MONOID with type t = string = {
  include Semigroup;
  let empty = "";
};

module Eq: EQ with type t = string = {
  type t = string;
  let eq = (==);
};
