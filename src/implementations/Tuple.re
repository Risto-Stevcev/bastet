open Interface;

let first = ((a, _)) => a
and second = ((_, b)) => b
and swap = (a, b) => (b, a)
and curry = (f, a, b) => f((a, b))
and uncurry = (f, (a, b)) => f(a, b);

module type MAGMA_F =
  (First: MAGMA, Second: MAGMA) => MAGMA with type t = (First.t, Second.t);
module type SEMIGROUP_F =
  (First: SEMIGROUP, Second: SEMIGROUP) =>
  SEMIGROUP with type t = (First.t, Second.t);
module type MONOID_F =
  (First: MONOID, Second: MONOID) => MONOID with type t = (First.t, Second.t);
module type FUNCTOR_F = (T: TYPE) => FUNCTOR with type t('a) = (T.t, 'a);
module type APPLY_F = (S: SEMIGROUP) => APPLY with type t('a) = (S.t, 'a);
module type APPLICATIVE_F =
  (M: MONOID) => APPLICATIVE with type t('a) = (M.t, 'a);
module type MONAD_F = (M: MONOID) => MONAD with type t('a) = (M.t, 'a);
module type FOLDABLE_F = (T: TYPE) => FOLDABLE with type t('a) = (T.t, 'a);
module type EQ_F =
  (First: EQ, Second: EQ) => EQ with type t = (First.t, Second.t);
module type SHOW_F =
  (First: SHOW, Second: SHOW) => SHOW with type t = (First.t, Second.t);
module type TRAVERSABLE_F =
  (T: TYPE, A: APPLICATIVE) =>
  TRAVERSABLE with
    type t('a) = (T.t, 'a) and type applicative_t('a) = A.t('a);

module Magma: MAGMA_F =
  (First: MAGMA, Second: MAGMA) => {
    type t = (First.t, Second.t);
    let append = ((a, b), (a', b')) => (
      First.append(a, a'),
      Second.append(b, b'),
    );
  };

module Semigroup: SEMIGROUP_F =
  (First: SEMIGROUP, Second: SEMIGROUP) => {
    include Magma(First, Second);
  };

module Monoid: MONOID_F =
  (First: MONOID, Second: MONOID) => {
    include Semigroup(First, Second);
    let empty = (First.empty, Second.empty);
  };

module Functor: FUNCTOR_F =
  (T: TYPE) => {
    type t('a) = (T.t, 'a);
    let map = (f, (a, b)) => (a, f(b));
  };

module Apply: APPLY_F =
  (S: SEMIGROUP) => {
    include Functor(S);
    let apply = ((a, f), (a', x)) => (S.append(a, a'), f(x));
  };

module Applicative: APPLICATIVE_F =
  (M: MONOID) => {
    include Apply(M);
    let pure = a => (M.empty, a);
  };

module Monad: MONAD_F =
  (M: MONOID) => {
    include Applicative(M);
    let flat_map = ((a, b), f) =>
      switch (f(b)) {
      | (a', c) => (M.append(a, a'), c)
      };
  };

module Foldable: FOLDABLE_F =
  (T: TYPE) => {
    type t('a) = (T.t, 'a);

    let fold_left = (f, init, (_, x)) => f(init, x)
    and fold_right = (f, init, (_, x)) => f(x, init);

    module Fold = {
      let fold_map = (f, (_, x)) => f(x);
    };

    module Fold_Map = (M: MONOID) => {
      include Fold;
    };
    module Fold_Map_Plus = (P: PLUS) => {
      include Fold;
    };
    module Fold_Map_Any = (M: MONOID_ANY) => {
      include Fold;
    };
  };

module Traversable: TRAVERSABLE_F =
  (T: TYPE, A: APPLICATIVE) => {
    type t('a) = (T.t, 'a)
    and applicative_t('a) = A.t('a);

    include (Functor(T): FUNCTOR with type t('a) := t('a));
    include (Foldable(T): FOLDABLE with type t('a) := t('a));

    let traverse = (f, (x, y)) => A.map(z => (x, z), f(y))
    and sequence = ((x, y)) => A.map(z => (x, z), y);
  };

module Eq: EQ_F =
  (First: EQ, Second: EQ) => {
    type t = (First.t, Second.t);
    let eq = ((a, b), (a', b')) => First.eq(a, a') && Second.eq(b, b');
  };

module Semigroupoid: SEMIGROUPOID with type t('a, 'b) = ('a, 'b) = {
  type t('a, 'b) = ('a, 'b);
  let compose = ((_, c), (a, _)) => (a, c);
};

module Show: SHOW_F =
  (First: SHOW, Second: SHOW) => {
    type t = (First.t, Second.t);
    let show = ((a, b)) =>
      "(" ++ First.show(a) ++ ", " ++ Second.show(b) ++ ")";
  };

module Bifunctor: BIFUNCTOR with type t('a, 'b) = ('a, 'b) = {
  type t('a, 'b) = ('a, 'b);
  let bimap = (f, g, (a, b)) => (f(a), g(b));
};

module Biapply: BIAPPLY with type t('a, 'b) = ('a, 'b) = {
  include Bifunctor;
  let biapply = ((f, g), (a, b)) => (f(a), g(b));
};

module Biapplicative: BIAPPLICATIVE with type t('a, 'b) = ('a, 'b) = {
  include Biapply;
  let bipure = (a, b) => (a, b);
};

module Bifoldable: BIFOLDABLE with type t('a, 'b) = ('a, 'b) = {
  type t('a, 'b) = ('a, 'b);

  let bifold_left = (f, g, init, (a, b)) => g(f(init, a), b)
  and bifold_right = (f, g, init, (a, b)) => f(a, g(b, init));

  module Fold_Map = (M: MONOID) => {
    let fold_map = (f, g, (a, b)) => M.append(f(a), g(b));
  };
  module Fold_Map_Any = (M: MONOID_ANY) => {
    let fold_map = (f, g, (a, b)) => M.append(f(a), g(b));
  };
  module Fold_Map_Plus = (P: PLUS) => {
    let fold_map = (f, g, (a, b)) => P.alt(f(a), g(b));
  };
};

module type BITRAVERSABLE_F =
  (A: APPLICATIVE) =>
  BITRAVERSABLE with
    type applicative_t('a) = A.t('a) and type t('a, 'b) = ('a, 'b);

module Bitraversable: BITRAVERSABLE_F =
  (A: APPLICATIVE) => {
    type t('a, 'b) = ('a, 'b)
    and applicative_t('a) = A.t('a);

    include (Bifunctor: BIFUNCTOR with type t('a, 'b) := t('a, 'b));
    include (Bifoldable: BIFOLDABLE with type t('a, 'b) := t('a, 'b));

    module I = Infix.Apply(A);
    let bitraverse = (f, g, (a, b)) =>
      I.(A.map((a, b) => (a, b), f(a)) <*> g(b))
    and bisequence = ((a, b)) => I.(A.map((a, b) => (a, b), a) <*> b);
  };

module Infix = {
  include Infix.Biapply(Biapply);
};
