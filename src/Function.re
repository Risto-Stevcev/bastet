open Interface;

module Semigroupoid: SEMIGROUPOID with type t('a, 'b) = 'a => 'b = {
  type t('a, 'b) = 'a => 'b;
  let compose = (f, g, x) => f(g(x));
};

module Category: CATEGORY with type t('a, 'b) = 'a => 'b = {
  include Semigroupoid;
  let id = (a) => a;
};

let flip: (('a, 'b) => 'c, 'b, 'a) => 'c = (f, b, a) => f(a, b);

let const: ('a, 'b) => 'a = (a, _) => a;

module Infix = {
  module I = Infix.Category(Category);
  include I;
};
