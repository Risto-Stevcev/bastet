open Interface;

let flip: (('a, 'b) => 'c, 'b, 'a) => 'c = (f, b, a) => f(a, b)
and const: ('a, 'b) => 'a = (a, _) => a;

module type FUNCTOR_F   = (T: TYPE) => FUNCTOR   with type t('a) = T.t => 'a;
module type APPLY_F     = (T: TYPE) => APPLY     with type t('a) = T.t => 'a;
module type INVARIANT_F = (T: TYPE) => INVARIANT with type t('a) = T.t => 'a;

module Functor: FUNCTOR_F = (T: TYPE) => {
  type t('b) = T.t => 'b;
  let map = (f, g, x) => f(g(x))
};

module Apply: APPLY_F = (T: TYPE) => {
  module Functor = Functor(T);
  include Functor;
  let apply = (f, g, x) => f(x, g(x))
};

module Semigroupoid: SEMIGROUPOID with type t('a, 'b) = 'a => 'b = {
  type t('a, 'b) = 'a => 'b;
  let compose = (f, g, x) => f(g(x))
};

module Category: CATEGORY with type t('a, 'b) = 'a => 'b = {
  include Semigroupoid;
  let id = (a) => a
};

module Invariant: INVARIANT_F = (T: TYPE) => {
  module F = Functor(T);
  type t('b) = T.t => 'b;
  let imap = (f, _) => F.map(f)
};

module Profunctor: PROFUNCTOR with type t('a, 'b) = 'a => 'b = {
  module I = Infix.Semigroupoid(Semigroupoid);
  let (>.) = I.(>.);
  type t('a, 'b) = 'a => 'b;
  let dimap = (a_to_b, c_to_d, b_to_c) => a_to_b >. b_to_c >. c_to_d
};

module Infix = {
  include Infix.Semigroupoid(Semigroupoid)
};
