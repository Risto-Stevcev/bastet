open Interface;

module Fn = Infix.Category(Function.Category);
open Fn;

/* A data structure representing the monoid of endomorphisms under composition (a => a) */
type endo('a) = Endo('a => 'a);


module Semigroup: SEMIGROUP_ANY with type t('a) = endo('a) = {
  type t('a) = endo('a);
  let append = (f, g) => switch (f, g) { | (Endo(f'), Endo(g')) => Endo(f' << g') };
};

module Monoid: MONOID_ANY with type t('a) = endo('a) = {
  include Semigroup;
  let empty = Endo(id);
};
