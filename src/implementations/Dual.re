open Interface;

/* A data structure representing the dual of a monoid */
type dual('a) = Dual('a);


module Magma = (M: MAGMA) => {
  module Dual_Magma_: MAGMA with type t = dual(M.t) = {
    type t = dual(M.t);
    let append = (a, b) => switch (a, b) {
      | (Dual(a'), Dual(b')) => Dual(M.append(b', a'));
      };
  };
  include Dual_Magma_;
};

module Semigroup = (S: SEMIGROUP) => {
  module Dual_Semigroup_: SEMIGROUP with type t = dual(S.t) = {
    include Magma(S);
  };
  include Dual_Semigroup_;
};

module Monoid = (M: MONOID) => {
  module Dual_Monoid_: MONOID with type t = dual(M.t) = {
    include Semigroup(M);
    let empty = Dual(M.empty);
  };
  include Dual_Monoid_;
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

module Magma_Any = (M: MAGMA_ANY) => {
  module Magma_Any_: MAGMA_ANY with type t('a) = dual(M.t('a)) = {
    type t('a) = dual(M.t('a));
    let append = (a, b) => switch (a, b) {
      | (Dual(a'), Dual(b')) => Dual(M.append(b', a'));
    };
  };
  include Magma_Any_;
};

module Semigroup_Any = (S: SEMIGROUP_ANY) => {
  module Semigroup_Any_: SEMIGROUP_ANY with type t('a) = dual(S.t('a)) = {
    include Magma_Any(S);
  };
  include Semigroup_Any_;
};

module Monoid_Any = (M: MONOID_ANY) => {
  module Monoid_Any_: MONOID_ANY with type t('a) = dual(M.t('a)) = {
    include Semigroup_Any(M);
    let empty = Dual(M.empty);
  };
  include Monoid_Any_;
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
  module Fold_Map_Plus = (P: PLUS) => {
    let fold_map = (f, x) => switch x { | Dual(x') => f(x') };
  };
};

module Traversable = (A: APPLICATIVE) => {
  module Dual_Traversable_: TRAVERSABLE
    with type applicative_t('a) = A.t('a) and type t('a) = dual('a) = {
    type t('a) = dual('a);
    type applicative_t('a) = A.t('a);
    include (Functor: FUNCTOR with type t('a) := t('a));
    include (Foldable: FOLDABLE with type t('a) := t('a));

    module I = Infix.Functor(A);
    let traverse = (f, x) => I.(switch x { | Dual(x') => ((x) => Dual(x)) <$> f(x') });
    let sequence = (x) => I.(switch x { | Dual(x') => ((x) => Dual(x)) <$> x' });
  };
  include Dual_Traversable_
};

module Infix = {
  include Infix.Monad(Monad)
};
