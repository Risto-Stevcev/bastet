open Interface;

module Magma: MAGMA with type t = Js.Date.t = {
  type t = Js.Date.t;
  let append = (a, b) => Js.Date.fromFloat(Js.Date.getTime(a) +. Js.Date.getTime(b))
};

module Medial_Magma: MEDIAL_MAGMA with type t = Js.Date.t = Magma;

module Semigroup: SEMIGROUP with type t = Js.Date.t = {
  include Magma
};

module Monoid: MONOID with type t = Js.Date.t = {
  include Semigroup;
  let empty = Js.Date.fromFloat(0.0)
};

module Eq: EQ with type t = Js.Date.t = {
  type t = Js.Date.t;
  let eq = (a, b) => Js.Date.getTime(a) == Js.Date.getTime(b)
};

module Ord: ORD with type t = Js.Date.t = {
  include Eq;
  let compare = (a, b) => unsafe_compare(Js.Date.getTime(a), Js.Date.getTime(b))
};

module Infix = {
  include Infix.Magma(Magma);
  include Infix.Eq(Eq);
  include Infix.Ord(Ord)
};
