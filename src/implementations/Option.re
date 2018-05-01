/* Option is the equivalent of Maybe in Haskell for Ocaml */
open Interface;
let (<.) = Function.Infix.(<.);

let maybe: (~f:'a => 'b, ~default:'b, option('a)) => 'b =
  (~f, ~default, opt) => switch opt {
  | Some(a) => f(a)
  | None => default
  };


module type MAGMA_F       = (M: MAGMA)       => MAGMA       with type t = option(M.t);
module type SEMIGROUP_F   = (S: SEMIGROUP)   => SEMIGROUP   with type t = option(S.t);
module type MONOID_F      = (S: SEMIGROUP)   => MONOID      with type t = option(S.t);
module type QUASIGROUP_F  = (Q: QUASIGROUP)  => QUASIGROUP  with type t = option(Q.t);
module type LOOP_F        = (L: LOOP)        => LOOP        with type t = option(L.t);
module type EQ_F          = (E: EQ)          => EQ          with type t = option(E.t);
module type SHOW_F        = (S: SHOW)        => SHOW        with type t = option(S.t);
module type TRAVERSABLE_F = (A: APPLICATIVE) => TRAVERSABLE with type t('a) = option('a)
  and type applicative_t('a) = A.t('a);

module Functor: FUNCTOR with type t('a) = option('a) = {
  type t('a) = option('a);
  let map = (f, a) => switch a {
    | Some(a') => Some(f(a'))
    | None => None
    }
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
  let pure = (a) => Some(a)
};

module Monad: MONAD with type t('a) = option('a) = {
  include Applicative;
  let flat_map = (x, f) => switch x {
    | Some(x') => f(x')
    | None => None
    }
};

module Magma: MAGMA_F = (M: MAGMA) => {
  type t = option(M.t);
  let append = (a, b) => switch (a, b) {
    | (Some(a), Some(b)) => Some(M.append(a, b))
    | (Some(a), _)
    | (_, Some(a)) => Some(a)
    | _ => None
    }
};

module Semigroup = (S: SEMIGROUP) => {
  include Magma(S)
};

module Monoid: MONOID_F = (S: SEMIGROUP) => {
  include Semigroup(S);
  let empty = None
};

module Quasigroup: QUASIGROUP_F = (Q: QUASIGROUP) => {
  include Magma(Q)
};

module Loop: LOOP_F = (L: LOOP) => {
  include Quasigroup(L);
  let empty = None
};

module Alt: ALT with type t('a) = option('a) = {
  include Functor;
  let alt = (a, b) => switch (a, b) {
    | (Some(a), _) => Some(a)
    | (None, a) => a
    }
};

module Plus: PLUS with type t('a) = option('a) = {
  include Alt;
  let empty = None
};

module Alternative: ALTERNATIVE with type t('a) = option('a) = {
  include Applicative;
  include (Plus: PLUS with type t('a) := t('a))
};

module Foldable: FOLDABLE with type t('a) = option('a) = {
  type t('a) = option('a);

  let fold_left  = (f, init, x) => maybe(~f=f(init), ~default=init, x)
  and fold_right = (f, init, x) => maybe(~f=(x') => f(x', init), ~default=init, x);

  module Fold_Map = (M: MONOID) => {
    let fold_map = (f, x) => maybe(~f, ~default=M.empty, x)
  };
  module Fold_Map_Any = (M: MONOID_ANY) => {
    let fold_map = (f, x) => maybe(~f, ~default=M.empty, x)
  };
  module Fold_Map_Plus = (P: PLUS) => {
    let fold_map = (f, x) => maybe(~f, ~default=P.empty, x)
  }
};

module Traversable = (A: APPLICATIVE) => {
  type t('a) = option('a) and applicative_t('a) = A.t('a);

  include (Functor: FUNCTOR with type t('a) := t('a));
  include (Foldable: FOLDABLE with type t('a) := t('a));

  let traverse = (f, x) => maybe(~f=A.map(a => Some(a)) <. f, ~default=A.pure(None), x)
  and sequence = (x) => maybe(~f=A.map(a => Some(a)), ~default=A.pure(None), x);
};

module Eq: EQ_F = (E: EQ) => {
  type t = option(E.t);
  let eq = (xs, ys) => switch (xs, ys) {
    | (Some(a), Some(b)) => E.eq(a, b)
    | (None, None) => true
    | _ => false
    }
};

module Show: SHOW_F = (S: SHOW) => {
  type t = option(S.t);
  let show = (a) => switch a {
    | Some(a') => "Some(" ++ S.show(a') ++ ")"
    | None => "None"
    }
};

module Infix = {
  include Infix.Monad(Monad);
  include Infix.Alternative(Alternative);
  let (|?) = Js.Option.getWithDefault
};
