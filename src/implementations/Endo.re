open Interface;
let (id) = Function.Category.id;
let (<.) = Function.Infix.(<.);

/* A data structure representing the monoid of endomorphisms under composition (a => a) */
type endo('a) = Endo('a => 'a);


module Magma: MAGMA_ANY with type t('a) = endo('a) = {
  type t('a) = endo('a);
  let append = (f, g) => switch (f, g) { | (Endo(f'), Endo(g')) => Endo(f' <. g') };
};

module Semigroup: SEMIGROUP_ANY with type t('a) = endo('a) = {
  include Magma;
};

module Monoid: MONOID_ANY with type t('a) = endo('a) = {
  include Semigroup;
  let empty = Endo(id);
};

module Infix = {
  include Infix.Magma_Any(Magma)
};
