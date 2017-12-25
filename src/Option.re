open Interface;

module Semigroup = (S: SEMIGROUP) => {
  module S: SEMIGROUP_ANY with type t('a) = option(S.t) = {
    type t('a) = option(S.t);
    let append = (a, b) => switch (a, b) {
      | (Some(a), Some(b)) => Some(S.append(a, b))
      | (Some(a), _)
      | (_, Some(a)) => Some(a)
      | _ => None
      };
  };
  include S;
};

module Monoid = (S: SEMIGROUP) => {
  module S' = Semigroup(S);
  module M: MONOID_ANY with type t('a) = option(S.t) = {
    include S';
    let empty = None;
  };
  include M;
};

module Functor: FUNCTOR with type t('a) = option('a) = {
  type t('a) = option('a);
  let map = (f, a) => switch a {
    | Some(a') => Some(f(a'))
    | None => None
    };
};

module Apply: APPLY with type t('a) = option('a) = {
  include Functor;
  let apply = (fn_opt, a) => switch fn_opt {
    | Some(f) => map(f, a)
    | None => None
    }
};

module Applicative: APPLICATIVE with type t('a) = option('a) = {
  include Apply;
  let pure = (a) => Some(a);
};

module Monad: MONAD with type t('a) = option('a) = {
  include Applicative;
  let flat_map = (x, f) => switch x {
    | Some(x') => f(x')
    | None => None
    };
};

module Foldable: FOLDABLE with type t('a) = option('a) = {
  type t('a) = option('a);
  let fold_left = (f, init, x) => switch x {
    | Some(x') => f(init, x')
    | None => init
    };
  let fold_right = (f, init, x) => switch x {
    | Some(x') => f(x', init)
    | None => init
    };

  module Fold_Map = (M: MONOID) => {
    let fold_map = (f, x) => switch x {
      | Some(x') => f(x')
      | None => M.empty
      }
  };

  module Fold_Map_Any = (M: MONOID_ANY) => {
    let fold_map = (f, x) => switch x {
      | Some(x') => f(x')
      | None => M.empty
      }
  };
};


module type TRAVERSABLE_F = (A: APPLICATIVE) => TRAVERSABLE
  with type applicative_t('a) = A.t('a) and type t('a) = option('a);

module Traversable: TRAVERSABLE_F = (A: APPLICATIVE) => {
  type t('a) = option('a);
  type applicative_t('a) = A.t('a);
  include (Functor: FUNCTOR with type t('a) := t('a));
  include (Foldable: FOLDABLE with type t('a) := t('a));

  module I = Infix.Functor(A);
  let traverse = (f, x) => I.(switch x {
    | Some(x') => (a => Some(a)) <$> f(x')
    | None => A.pure(None)
    });

  let sequence = (x) => I.(switch x {
    | Some(x') => (a => Some(a)) <$> x'
    | None => A.pure(None)
  });
};


module type EQ1_F = (E: EQ) => EQ1 with type t('a) = option(E.t);

module Eq: EQ1_F = (E: EQ) => {
  type t('a) = option(E.t);
  let eq = (xs, ys) => switch (xs, ys) {
    | (Some(a), Some(b)) => E.eq(a, b)
    | (None, None) => true
    | _ => false
    };
};
