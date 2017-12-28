open Interface;

let first = ((a, _)) => a;
let second = ((_, b)) => b;
let swap = (a, b) => (b, a);
let curry = (f, a, b) => f((a, b));
let uncurry = (f, (a, b)) => f(a, b);

module Magma = (First: MAGMA, Second: MAGMA) => {
  module Tuple_Magma_: MAGMA with type t = (First.t, Second.t) = {
    type t = (First.t, Second.t);
    let append = ((a, b), (a', b')) => (First.append(a, a'), Second.append(b, b'));
  };
  include Tuple_Magma_;
};

module Semigroup = (First: SEMIGROUP, Second: SEMIGROUP) => {
  module Tuple_Semigroup_: SEMIGROUP with type t = (First.t, Second.t) = {
    include Magma(First, Second);
  };
  include Tuple_Semigroup_;
};

module Monoid = (First: MONOID, Second: MONOID) => {
  module Tuple_Monoid_: MONOID with type t = (First.t, Second.t) = {
    include Semigroup(First, Second);
    let empty = (First.empty, Second.empty)
  };
  include Tuple_Monoid_;
};

module Functor = (T: TYPE) => {
  module Tuple_Functor_: FUNCTOR with type t('a) = (T.t, 'a) = {
    type t('a) = (T.t, 'a);
    let map = (f, (a, b)) => (a, f(b));
  };
  include Tuple_Functor_;
};

module Apply = (S: SEMIGROUP) => {
  module Tuple_Apply_: APPLY with type t('a) = (S.t, 'a) = {
    include Functor(S);
    let apply = ((a, f), (a', x)) => (S.append(a, a'), f(x));
  };
  include Tuple_Apply_;
};

module Applicative = (M: MONOID) => {
  module Tuple_Applicative_: APPLICATIVE with type t('a) = (M.t, 'a) = {
    include Apply(M);
    let pure = (a) => (M.empty, a);
  };
  include Tuple_Applicative_;
};

module Monad = (M: MONOID) => {
  module Tuple_Monad_: MONAD with type t('a) = (M.t, 'a) = {
    include Applicative(M);
    let flat_map = ((a, b), f) => switch (f(b)) {
      | (a', c) => (M.append(a, a'), c)
      }
  };
  include Tuple_Monad_
};

module Foldable = (T: TYPE) => {
  module Tuple_Foldable_: FOLDABLE with type t('a) = (T.t, 'a) = {
    type t('a) = (T.t, 'a);
    let fold_left = (f, init, (_, x)) => f(init, x);
    let fold_right = (f, init, (_ , x)) => f(x, init);

    module Fold_Map = (M: MONOID) => {
      let fold_map = (f, (_, x)) => f(x);
    };
    module Fold_Map_Plus = (P: PLUS) => {
      let fold_map = (f, (_, x)) => f(x);
    };
    module Fold_Map_Any = (M: MONOID_ANY) => {
      let fold_map = (f, (_, x)) => f(x);
    };
  };
  include Tuple_Foldable_;
};

module Traversable = (T: TYPE, A: APPLICATIVE) => {
  module Tuple_Traversable_: TRAVERSABLE
    with type applicative_t('a) = A.t('a) and type t('a) = (T.t, 'a) = {
    type t('a) = (T.t, 'a);
    type applicative_t('a) = A.t('a);
    include (Functor(T): FUNCTOR with type t('a) := t('a));
    include (Foldable(T): FOLDABLE with type t('a) := t('a));

    let traverse = (f, (x, y)) => A.map(z => (x, z), f(y));
    let sequence = ((x, y)) => A.map(z => (x, z), y);
  };
  include Tuple_Traversable_;
};

module Eq = (First: EQ, Second: EQ) => {
  module Tuple_Eq_: EQ with type t = (First.t, Second.t) = {
    type t = (First.t, Second.t);
    let eq = ((a, b), (a', b')) => First.eq(a, a') && Second.eq(b, b');
  };
  include Tuple_Eq_;
};

module Semigroupoid: SEMIGROUPOID with type t('a, 'b) = ('a, 'b) = {
  type t('a, 'b) = ('a, 'b);
  let compose = ((_, c), (a, _)) => (a, c);
};

module Show = (First: SHOW, Second: SHOW) => {
  module Tuple_Show_: SHOW with type t = (First.t, Second.t) = {
    type t = (First.t, Second.t);
    let show = ((a, b)) => "(" ++ First.show(a) ++ ", " ++ Second.show(b) ++ ")";
  };
  include Tuple_Show_;
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
  let bifold_left = (f, g, init, (a, b)) => g(f(init, a), b);
  let bifold_right = (f, g, init, (a, b)) => f(a, g(b, init));

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

module Bitraversable = (A: APPLICATIVE) => {
  module Tuple_Bitraversable_: BITRAVERSABLE
    with type applicative_t('a) = A.t('a) and type t('a, 'b) = ('a, 'b) = {
    module I = Infix.Apply(A);
    type t('a, 'b) = ('a, 'b);
    type applicative_t('a) = A.t('a);
    include (Bifunctor: BIFUNCTOR with type t('a, 'b) := t('a, 'b));
    include (Bifoldable: BIFOLDABLE with type t('a, 'b) := t('a, 'b));

    let bitraverse = (f, g, (a, b)) => I.(A.map((a, b) => (a, b), f(a)) <*> g(b));
    let bisequence = ((a, b)) => I.(A.map((a, b) => (a, b), a) <*> b);
  };
  include Tuple_Bitraversable_;
};
