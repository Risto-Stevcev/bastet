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

module Infix = {
  module Semigroupoid = Infix.Semigroupoid(Semigroupoid);
  include Semigroupoid;
};
