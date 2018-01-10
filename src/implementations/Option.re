/* Option is the equivalent of Maybe in Haskell for Ocaml */
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

module Magma = (M: MAGMA) => {
  module Option_Magma_: MAGMA with type t = option(M.t) = {
    type t = option(M.t);
    let append = (a, b) => switch (a, b) {
      | (Some(a), Some(b)) => Some(M.append(a, b))
      | (Some(a), _)
      | (_, Some(a)) => Some(a)
      | _ => None
      };
  };
  include Option_Magma_;
};

module Semigroup = (S: SEMIGROUP) => {
  module Option_Semigroup_: SEMIGROUP with type t = option(S.t) = {
    include Magma(S);
  };
  include Option_Semigroup_;
};

module Monoid = (S: SEMIGROUP) => {
  module Option_Monoid_: MONOID with type t = option(S.t) = {
    include Semigroup(S);
    let empty = None;
  };
  include Option_Monoid_;
};

module Quasigroup = (Q: QUASIGROUP) => {
  module Option_Quasigroup_: QUASIGROUP with type t = option(Q.t) = {
    include Magma(Q);
  };
  include Option_Quasigroup_;
};

module Loop = (L: LOOP) => {
  module Option_Loop_: LOOP with type t = option(L.t) = {
    include Quasigroup(L);
    let empty = None;
  };
  include Option_Loop_;
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

module Traversable = (A: APPLICATIVE) => {
  module Option_Traversable_: TRAVERSABLE
    with type applicative_t('a) = A.t('a) and type t('a) = option('a) = {
    type t('a) = option('a);
    type applicative_t('a) = A.t('a);
    include (Functor: FUNCTOR with type t('a) := t('a));
    include (Foldable: FOLDABLE with type t('a) := t('a));

    let traverse = (f, x) => maybe(~f=A.map(a => Some(a)) <. f, ~default=A.pure(None), x);
    let sequence = (x) => maybe(~f=A.map(a => Some(a)), ~default=A.pure(None), x);
  };
  include Option_Traversable_;
};

module Eq = (E: EQ) => {
  module Option_Eq_: EQ with type t = option(E.t) = {
    type t = option(E.t);
    let eq = (xs, ys) => switch (xs, ys) {
      | (Some(a), Some(b)) => E.eq(a, b)
      | (None, None) => true
      | _ => false
      };
  };
  include Option_Eq_;
};

module Show = (S: SHOW) => {
  module Option_Show_: SHOW with type t = option(S.t) = {
    type t = option(S.t);
    let show = (a) => switch a {
      | Some(a') => "Some(" ++ S.show(a') ++ ")"
      | None => "None"
      }
  };
  include Option_Show_;
};

module Infix = {
  include Infix.Monad(Monad);
  include Infix.Alternative(Alternative);
};
