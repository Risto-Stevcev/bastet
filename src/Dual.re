open Interface;

/* A data structure representing the dual of a monoid */
type dual('a) = Dual('a);

module Semigroup = (S: SEMIGROUP) => {
  module Semigroup: SEMIGROUP_ANY with type t('a) = dual(S.t) = {
    type t('a) = dual(S.t);
    let append = (a, b) => switch (a, b) {
      | (Dual(a'), Dual(b')) => Dual(S.append(b', a'));
    };
  };
  include Semigroup;
};

module Semigroup_Any = (S: SEMIGROUP_ANY) => {
  module Semigroup_Any: SEMIGROUP_ANY with type t('a) = dual(S.t('a)) = {
    type t('a) = dual(S.t('a));
    let append = (a, b) => switch (a, b) {
      | (Dual(a'), Dual(b')) => Dual(S.append(b', a'));
    };
  };
  include Semigroup_Any;
};

module Monoid = (M: MONOID) => {
  module Monoid: MONOID_ANY with type t('a) = dual(M.t) = {
    module S = Semigroup(M);
    include S;
    let empty = Dual(M.empty);
  };
  include Monoid;
};

module Monoid_Any = (M: MONOID_ANY) => {
  module Monoid_Any: MONOID_ANY with type t('a) = dual(M.t('a)) = {
    module S = Semigroup_Any(M);
    include S;
    let empty = Dual(M.empty);
  };
  include Monoid_Any;
};

module Functor: FUNCTOR with type t('a) = dual('a) = {
  type t('a) = dual('a);
  let map = (f, a) => switch a { | Dual(a') => Dual(f(a')) };
};

module Applicative: APPLICATIVE with type t('a) = dual('a) = {
  include Functor;
  let apply = (f, a) => switch (f, a) { | (Dual(f'), Dual(a')) => Dual(f'(a')) };
  let pure = (a) => Dual(a);
};

module Monad: MONAD with type t('a) = dual('a) = {
  include Applicative;
  let flat_map = (a, f) => switch a { | Dual(a') => f(a') };
};
