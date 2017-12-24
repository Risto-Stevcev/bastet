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

module Foldable: FOLDABLE with type t('a) = dual('a) = {
  type t('a) = dual('a);
  let fold_left = (f, init, x) => switch x { | Dual(x') => f(init, x') };
  let fold_right = (f, init, x) => switch x { | Dual(x') => f(x', init) };

  module Fold_Map = (M: MONOID) => {
    let fold_map = (f, x) => switch x { | Dual(x') => f(x') };
  };

  module Fold_Map_Any = (M: MONOID_ANY) => {
    let fold_map = (f, x) => switch x { | Dual(x') => f(x') };
  };
};

module type TRAVERSABLE_F = (A: APPLICATIVE) => TRAVERSABLE
  with type applicative_t('a) = A.t('a) and type t('a) = dual('a);

module Traversable: TRAVERSABLE_F = (A: APPLICATIVE) => {
  type t('a) = dual('a);
  type applicative_t('a) = A.t('a);
  include (Functor: FUNCTOR with type t('a) := t('a));
  include (Foldable: FOLDABLE with type t('a) := t('a));

  module I = Infix.Functor(A);
  let traverse = (f, x) => I.(switch x { | Dual(x') => ((x) => Dual(x)) <$> f(x') });
  let sequence = (x) => I.(switch x { | Dual(x') => ((x) => Dual(x)) <$> x' });
};
