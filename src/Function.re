open Interface;

module Semigroupoid: SEMIGROUPOID with type t('a, 'b) = 'a => 'b = {
  type t('a, 'b) = 'a => 'b;
  let compose = (f, g, x) => f(g(x));
};

module Category: CATEGORY with type t('a, 'b) = 'a => 'b = {
  include Semigroupoid;
  let id = (a) => a;
};
