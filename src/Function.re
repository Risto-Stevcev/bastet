open Interface;

let flip: (('a, 'b) => 'c, 'b, 'a) => 'c = (f, b, a) => f(a, b);

let const: ('a, 'b) => 'a = (a, _) => a;


module Functor = (T: TYPE) => {
  module Function_Functor: FUNCTOR with type t('b) = T.t => 'b = {
    type t('b) = T.t => 'b;
    let map = (f, g, x) => f(g(x));
  };
  include Function_Functor;
};


module Semigroupoid: SEMIGROUPOID with type t('a, 'b) = 'a => 'b = {
  type t('a, 'b) = 'a => 'b;
  let compose = (f, g, x) => f(g(x));
};

module Category: CATEGORY with type t('a, 'b) = 'a => 'b = {
  include Semigroupoid;
  let id = (a) => a;
};

module Invariant = (T: TYPE) => {
  module F = Functor(T);
  module Function_Invariant: INVARIANT with type t('b) = T.t => 'b = {
    type t('b) = T.t => 'b;
    let imap = (f, _) => F.map(f);
  };
  include Function_Invariant;
};

module Profunctor: PROFUNCTOR with type t('a, 'b) = 'a => 'b = {
  module I = Infix.Semigroupoid(Semigroupoid);
  let (>.) = I.(>.);
  type t('a, 'b) = 'a => 'b;
  let dimap = (a_to_b, c_to_d, b_to_c) => a_to_b >. b_to_c >. c_to_d;
};

module Infix = {
  module Semigroupoid = Infix.Semigroupoid(Semigroupoid);
  include Semigroupoid;
};
