open Interface;

/* A data structure representing the dual of a monoid */
type dual('a) =
  | Dual('a);

module type MAGMA_F = (M: MAGMA) => MAGMA with type t = dual(M.t);
module type SEMIGROUP_F =
  (S: SEMIGROUP) => SEMIGROUP with type t = dual(S.t);
module type MONOID_F = (M: MONOID) => MONOID with type t = dual(M.t);
module type MAGMA_ANY_F =
  (M: MAGMA_ANY) => MAGMA_ANY with type t('a) = dual(M.t('a));
module type SEMIGROUP_ANY_F =
  (S: SEMIGROUP_ANY) => SEMIGROUP_ANY with type t('a) = dual(S.t('a));
module type MONOID_ANY_F =
  (M: MONOID_ANY) => MONOID_ANY with type t('a) = dual(M.t('a));
module type TRAVERSABLE_F =
  (A: APPLICATIVE) =>
  TRAVERSABLE with
    type t('a) = dual('a) and type applicative_t('a) = A.t('a);

module Magma: MAGMA_F =
  (M: MAGMA) => {
    type t = dual(M.t);
    let append = (Dual(a), Dual(b)) => Dual(M.append(b, a));
  };

module Semigroup: SEMIGROUP_F =
  (S: SEMIGROUP) => {
    include Magma(S);
  };

module Monoid: MONOID_F =
  (M: MONOID) => {
    include Semigroup(M);
    let empty = Dual(M.empty);
  };

module Functor: FUNCTOR with type t('a) = dual('a) = {
  type t('a) = dual('a);
  let map = (f, Dual(a)) => Dual(f(a));
};

module Applicative: APPLICATIVE with type t('a) = dual('a) = {
  include Functor;
  let apply = (Dual(f), Dual(a)) => Dual(f(a));
  let pure = a => Dual(a);
};

module Monad: MONAD with type t('a) = dual('a) = {
  include Applicative;
  let flat_map = (Dual(a), f) => f(a);
};

module Magma_Any: MAGMA_ANY_F =
  (M: MAGMA_ANY) => {
    type t('a) = dual(M.t('a));
    let append = (Dual(a), Dual(b)) => Dual(M.append(b, a));
  };

module Semigroup_Any: SEMIGROUP_ANY_F =
  (S: SEMIGROUP_ANY) => {
    include Magma_Any(S);
  };

module Monoid_Any: MONOID_ANY_F =
  (M: MONOID_ANY) => {
    include Semigroup_Any(M);
    let empty = Dual(M.empty);
  };

module Foldable: FOLDABLE with type t('a) = dual('a) = {
  type t('a) = dual('a);

  let fold_left = (f, init, Dual(x)) => f(init, x)
  and fold_right = (f, init, Dual(x)) => f(x, init);

  module Fold = {
    let fold_map = (f, Dual(x)) => f(x);
  };

  module Fold_Map = (M: MONOID) => {
    include Fold;
  };
  module Fold_Map_Any = (M: MONOID_ANY) => {
    include Fold;
  };
  module Fold_Map_Plus = (P: PLUS) => {
    include Fold;
  };
};

module Traversable: TRAVERSABLE_F =
  (A: APPLICATIVE) => {
    type t('a) = dual('a)
    and applicative_t('a) = A.t('a);

    include (Functor: FUNCTOR with type t('a) := t('a));
    include (Foldable: FOLDABLE with type t('a) := t('a));

    module I = Infix.Functor(A);
    let traverse = (f, x) =>
      I.(
        switch (x) {
        | Dual(x') => (x => Dual(x)) <$> f(x')
        }
      );
    let sequence = x =>
      I.(
        switch (x) {
        | Dual(x') => (x => Dual(x)) <$> x'
        }
      );
  };

module Infix = {
  include Infix.Monad(Monad);
};
