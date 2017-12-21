open Interface;

module Semigroup = (S: SEMIGROUP) => {
  module S: SEMIGROUP_ANY with type t('a) = option(S.t) = {
    type t('a) = option(S.t);
    let append = (a, b) =>
      switch (a, b) {
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
  let map = (f, a) =>
    switch a {
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
