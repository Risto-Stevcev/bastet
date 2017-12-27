open Interface;

let first = ((a, _)) => a;
let second = ((_, b)) => b;
let swap = (a, b) => (b, a);
let curry = (f, a, b) => f((a, b));
let uncurry = (f, (a, b)) => f(a, b);

module Semigroup = (First: SEMIGROUP, Second: SEMIGROUP) => {
  module Tuple_Semigroup: SEMIGROUP with type t = (First.t, Second.t) = {
    type t = (First.t, Second.t);
    let append = ((a, b), (a', b')) => (First.append(a, a'), Second.append(b, b'));
  };
  include Tuple_Semigroup;
};

module Monoid = (First: MONOID, Second: MONOID) => {
  module Tuple_Semigroup = Semigroup(First, Second);
  module Tuple_Monoid: MONOID with type t = (First.t, Second.t) = {
    include Tuple_Semigroup;
    let empty = (First.empty, Second.empty)
  };
  include Tuple_Monoid;
};

module Functor = (T: TYPE) => {
  module Tuple_Functor: FUNCTOR with type t('a) = (T.t, 'a) = {
    type t('a) = (T.t, 'a);
    let map = (f, (a, b)) => (a, f(b));
  };
  include Tuple_Functor;
};

module Apply = (S: SEMIGROUP) => {
  module Tuple_Functor = Functor(S);
  module Tuple_Apply: APPLY with type t('a) = (S.t, 'a) = {
    include Tuple_Functor;
    let apply = ((a, f), (a', x)) => (S.append(a, a'), f(x));
  };
  include Tuple_Apply;
};

module Applicative = (M: MONOID) => {
  module Tuple_Apply = Apply(M);
  module Tuple_Applicative: APPLICATIVE with type t('a) = (M.t, 'a) = {
    include Tuple_Apply;
    let pure = (a) => (M.empty, a);
  };
  include Tuple_Applicative
};

module Monad = (M: MONOID) => {
  module Tuple_Applicative = Applicative(M);
  module Tuple_Monad: MONAD with type t('a) = (M.t, 'a) = {
    include Tuple_Applicative;
    let flat_map = ((a, b), f) => switch (f(b)) {
      | (a', c) => (M.append(a, a'), c)
      }
  };
  include Tuple_Monad
};

module Foldable = (T: TYPE) => {
  module Tuple_Foldable: FOLDABLE with type t('a) = (T.t, 'a) = {
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
  include Tuple_Foldable
};

module Traversable = (T: TYPE, A: APPLICATIVE) => {
  module Tuple_Functor = Functor(T);
  module Tuple_Foldable = Foldable(T);
  module Tuple_Traversable: TRAVERSABLE
    with type applicative_t('a) = A.t('a) and type t('a) = (T.t, 'a) = {
    type t('a) = (T.t, 'a);
    type applicative_t('a) = A.t('a);
    include (Tuple_Functor: FUNCTOR with type t('a) := t('a));
    include (Tuple_Foldable: FOLDABLE with type t('a) := t('a));

    let traverse = (f, (x, y)) => A.map(z => (x, z), f(y));
    let sequence = ((x, y)) => A.map(z => (x, z), y);
  };
  include Tuple_Traversable
};

module Eq = (First: EQ, Second: EQ) => {
  module Tuple_Eq: EQ with type t = (First.t, Second.t) = {
    type t = (First.t, Second.t);
    let eq = ((a, b), (a', b')) => First.eq(a, a') && Second.eq(b, b');
  };
  include Tuple_Eq;
};

module Semigroupoid: SEMIGROUPOID with type t('a, 'b) = ('a, 'b) = {
  type t('a, 'b) = ('a, 'b);
  let compose = ((_, c), (a, _)) => (a, c);
};

module Show = (First: SHOW, Second: SHOW) => {
  module Tuple_Show: SHOW with type t = (First.t, Second.t) = {
    type t = (First.t, Second.t);
    let show = ((a, b)) => "(" ++ First.show(a) ++ ", " ++ Second.show(b) ++ ")";
  };
  include Tuple_Show;
};
