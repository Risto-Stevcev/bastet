open Interface;
let (<.) = Function.Infix.(<.);


let maybe: (~f:'a => 'b, ~default:'b, option('a)) => 'b =
  (~f, ~default, opt) => switch opt {
  | Some(a) => f(a)
  | None => default
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


module Semigroup = (S: SEMIGROUP) => {
  module Option_Semigroup: SEMIGROUP with type t = option(S.t) = {
    type t = option(S.t);
    let append = (a, b) => switch (a, b) {
      | (Some(a), Some(b)) => Some(S.append(a, b))
      | (Some(a), _)
      | (_, Some(a)) => Some(a)
      | _ => None
      };
  };
  include Option_Semigroup;
};


module Monoid = (S: SEMIGROUP) => {
  module Option_Semigroup = Semigroup(S);
  module Option_Monoid: MONOID with type t = option(S.t) = {
    include Option_Semigroup;
    let empty = None;
  };
  include Option_Monoid;
};


module Alt: ALT with type t('a) = option('a) = {
  include Functor;
  let alt = (a, b) => switch (a, b) {
    | (Some(a), _) => Some(a)
    | (None, a) => a
    };
};


module Plus: PLUS with type t('a) = option('a) = {
  include Alt;
  let empty = None;
};


module Alternative: ALTERNATIVE with type t('a) = option('a) = {
  include Applicative;
  include (Plus: PLUS with type t('a) := t('a));
};


module Foldable: FOLDABLE with type t('a) = option('a) = {
  type t('a) = option('a);
  let fold_left = (f, init, x) => maybe(~f=f(init), ~default=init, x);
  let fold_right = (f, init, x) => maybe(~f=(x') => f(x', init), ~default=init, x);

  module Fold_Map = (M: MONOID) => {
    let fold_map = (f, x) => maybe(~f, ~default=M.empty, x)
  };
  module Fold_Map_Any = (M: MONOID_ANY) => {
    let fold_map = (f, x) => maybe(~f, ~default=M.empty, x)
  };
  module Fold_Map_Plus = (P: PLUS) => {
    let fold_map = (f, x) => maybe(~f, ~default=P.empty, x)
  };
};


module type TRAVERSABLE_F = (A: APPLICATIVE) => TRAVERSABLE
  with type applicative_t('a) = A.t('a) and type t('a) = option('a);

module Traversable: TRAVERSABLE_F = (A: APPLICATIVE) => {
  type t('a) = option('a);
  type applicative_t('a) = A.t('a);
  include (Functor: FUNCTOR with type t('a) := t('a));
  include (Foldable: FOLDABLE with type t('a) := t('a));

  let traverse = (f, x) => maybe(~f=A.map(a => Some(a)) <. f, ~default=A.pure(None), x);
  let sequence = (x) => maybe(~f=A.map(a => Some(a)), ~default=A.pure(None), x);
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
