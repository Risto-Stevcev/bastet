/* Result is the equivalent of Either in Haskell for Ocaml */
open Interface;
open Belt.Result;
let (flip, const) = Function.(flip, const);

let result: ('a => 'c, 'b => 'c, Belt.Result.t('a, 'b)) => 'c = (f, g, a) => switch (f, g, a) {
  | (f, _, Ok(a')) => f(a')
  | (_, g, Error(a')) => g(a')
  };


module type MAGMA_F         = (T: TYPE, M: MAGMA)           => MAGMA         with type t = Belt.Result.t(M.t, T.t);
module type MEDIAL_MAGMA_F  = (T: TYPE, M: MAGMA)           => MEDIAL_MAGMA  with type t = Belt.Result.t(M.t, T.t);
module type SEMIGROUP_F     = (T: TYPE, S: SEMIGROUP)       => SEMIGROUP     with type t = Belt.Result.t(S.t, T.t);
module type FUNCTOR_F       = (T: TYPE)                     => FUNCTOR       with type t('a) = Belt.Result.t('a, T.t);
module type APPLY_F         = (T: TYPE)                     => APPLY         with type t('a) = Belt.Result.t('a, T.t);
module type APPLICATIVE_F   = (T: TYPE)                     => APPLICATIVE   with type t('a) = Belt.Result.t('a, T.t);
module type MONAD_F         = (T: TYPE)                     => MONAD         with type t('a) = Belt.Result.t('a, T.t);
module type ALT_F           = (T: TYPE)                     => ALT           with type t('a) = Belt.Result.t('a, T.t);
module type EXTEND_F        = (T: TYPE)                     => EXTEND        with type t('a) = Belt.Result.t('a, T.t);
module type SHOW_F          = (Ok: SHOW, Error: SHOW)       => SHOW          with type t = Belt.Result.t(Ok.t, Error.t);
module type EQ_F            = (Ok: EQ, Error: EQ)           => EQ            with type t = Belt.Result.t(Ok.t, Error.t);
module type ORD_F           = (Ok: ORD, Error: ORD)         => ORD           with type t = Belt.Result.t(Ok.t, Error.t);
module type BOUNDED_F       = (Ok: BOUNDED, Error: BOUNDED) => BOUNDED       with type t = Belt.Result.t(Ok.t, Error.t);
module type FOLDABLE_F      = (T: TYPE)                     => FOLDABLE      with type t('a) = Belt.Result.t('a, T.t);
module type TRAVERSABLE_F   = (T: TYPE, A: APPLICATIVE)     => TRAVERSABLE   with type t('a) = Belt.Result.t('a, T.t);
module type BITRAVERSABLE_F = (A: APPLICATIVE)              => BITRAVERSABLE with type t('a, 'b) = Belt.Result.t('a, 'b)
  and type applicative_t('a) = A.t('a);

module Magma: MAGMA_F = (T: TYPE, M: MAGMA) => {
  type t = Belt.Result.t(M.t, T.t);
  let append = (a, b) => switch (a, b) {
    | (Ok(a'), Ok(b')) => Ok(M.append(a', b'))
    | (_, Ok(b')) => Ok(b')
    | (Ok(a'), _) => Ok(a')
    | (Error(a'), _) => Error(a')
    }
};

module Medial_Magma: MEDIAL_MAGMA_F = (T: TYPE, M: MAGMA) => { include Magma(T, M) };

module Semigroup: SEMIGROUP_F = (T: TYPE, S: SEMIGROUP) => { include Magma(T, S) };

module Functor: FUNCTOR_F = (T: TYPE) => {
  type t('a) = Belt.Result.t('a, T.t);
  let map = (f, a) => switch a {
    | Ok(r) => Ok(f(r))
    | Error(l) => Error(l)
    }
};

module Bifunctor: BIFUNCTOR with type t('a, 'b) = Belt.Result.t('a, 'b) = {
  type t('a, 'b) = Belt.Result.t('a, 'b);
  let bimap = (f, g, a) => switch a {
    | Ok(a') => Ok(f(a'))
    | Error(a') => Error(g(a'))
    }
};

module Apply: APPLY_F = (T: TYPE) => {
  include Functor(T);
  let apply = (f, a) => switch (f, a) {
    | (Ok(f'), a') => map(f', a')
    | (Error(f'), _) => Error(f')
    }
};

module Applicative: APPLICATIVE_F = (T: TYPE) => {
  include Apply(T);
  let pure = a => Ok(a)
};

module Monad: MONAD_F = (T: TYPE) => {
  include Applicative(T);
  let flat_map = (a, f) => switch a {
    | Ok(a') => f(a')
    | Error(a') => Error(a')
    }
};

module Alt: ALT_F = (T: TYPE) => {
  include Functor(T);
  let alt = (a, b) => switch (a, b) {
    | (Error(_), b') => b'
    | (a', _) => a'
    }
};

module Extend: EXTEND_F = (T: TYPE) => {
  include Monad(T);
  let extend = (f, a) => switch (f, a) {
    | (_, Error(a')) => Error(a')
    | (f', a') => Ok(f'(a'))
    }
};

module Show: SHOW_F = (Ok: SHOW, Error: SHOW) => {
  type t = Belt.Result.t(Ok.t, Error.t);
  let show = result(Ok.show, Error.show)
};

module Eq: EQ_F = (Ok: EQ, Error: EQ) => {
  type t = Belt.Result.t(Ok.t, Error.t);
  let eq = (a, b) => switch (a, b) {
    | (Ok(a'), Ok(b')) => Ok.eq(a', b')
    | (Error(a'), Error(b')) => Error.eq(a', b')
    | _ => false
    }
};

module Ord: ORD_F = (Ok: ORD, Error: ORD) => {
  include Eq(Ok, Error);
  let compare = (a, b) => switch (a, b) {
    | (Ok(a'), Ok(b')) => Ok.compare(a', b')
    | (Error(a'), Error(b')) => Error.compare(a', b')
    | (Error(_), Ok(_)) => `less_than
    | (Ok(_), Error(_)) => `greater_than
    }
};

module Bounded: BOUNDED_F = (Ok: BOUNDED, Error: BOUNDED) => {
  include Ord(Ok, Error);
  let top = Ok(Ok.top);
  let bottom = Error(Error.bottom)
};

module Many_Valued_Logic = {
  /* Many valued logics in general have to relax certain constraints in order to 
   * be heyting or boolean algebras, such as:
   * - The reflexivity conditions of equivalence and implication (may be quasi-reflexive)
   * - The law of excluded middle
   */

  module type EQ_F  = (Ok: TYPE, Error: TYPE) => EQ  with type t = Belt.Result.t(Ok.t, Error.t);
  module type ORD_F = (Ok: TYPE, Error: TYPE) => ORD with type t = Belt.Result.t(Ok.t, Error.t);
  module type JOIN_SEMILATTICE_F = (Ok: JOIN_SEMILATTICE, Error: JOIN_SEMILATTICE) =>
    JOIN_SEMILATTICE with type t = Belt.Result.t(Ok.t, Error.t);
  module type MEET_SEMILATTICE_F = (Ok: MEET_SEMILATTICE, Error: MEET_SEMILATTICE) =>
    MEET_SEMILATTICE with type t = Belt.Result.t(Ok.t, Error.t);
  module type BOUNDED_JOIN_SEMILATTICE_F = (Ok: BOUNDED_JOIN_SEMILATTICE, Error: BOUNDED_JOIN_SEMILATTICE) => 
    BOUNDED_JOIN_SEMILATTICE with type t = Belt.Result.t(Ok.t, Error.t);
  module type BOUNDED_MEET_SEMILATTICE_F = (Ok: BOUNDED_MEET_SEMILATTICE, Error: BOUNDED_MEET_SEMILATTICE) =>
    BOUNDED_MEET_SEMILATTICE with type t = Belt.Result.t(Ok.t, Error.t);
  module type HEYTING_ALGEBRA_F = (Ok: HEYTING_ALGEBRA, Error: HEYTING_ALGEBRA) => 
    HEYTING_ALGEBRA with type t = Belt.Result.t(Ok.t, Error.t);

  module Quasireflexive_Eq: EQ_F = (Ok: TYPE, Error: TYPE) => {
    type t = Belt.Result.t(Ok.t, Error.t);
    /* Quasi-reflexive */
    let eq = (a, b) => switch (a, b) {
      | (Ok(_), Ok(_)) | (Error(_), Error(_)) => true 
      | _ => false
      }
  };

  module Quasireflexive_Ord: ORD_F = (Ok: TYPE, Error: TYPE) => {
    include Quasireflexive_Eq(Ok, Error);
    /* Quasi-reflexive */
    let compare = (a, b) => switch (a, b) {
      | (Ok(_), Ok(_)) | (Error(_), Error(_)) => `equal_to 
      | (Error(_), Ok(_)) => `less_than
      | (Ok(_), Error(_)) => `greater_than
      }
  };

  module Join_Semilattice: JOIN_SEMILATTICE_F = (Ok: JOIN_SEMILATTICE, Error: JOIN_SEMILATTICE) => {
    type t = Belt.Result.t(Ok.t, Error.t);
    let join = (a, b) => switch (a, b) {
      | (Ok(a'), Ok(b')) => Ok(Ok.join(a', b'))
      | (Ok(a'), _) | (_, Ok(a')) => Ok(a')
      | (Error(a'), Error(b')) => Error(Error.join(a', b'))
      }
  };

  module Meet_Semilattice: MEET_SEMILATTICE_F = (Ok: MEET_SEMILATTICE, Error: MEET_SEMILATTICE) => {
    type t = Belt.Result.t(Ok.t, Error.t);
    let meet = (a, b) => switch (a, b) {
      | (Ok(a'), Ok(b')) => Ok(Ok.meet(a', b'))
      | (Error(a'), Error(b')) => Error(Error.meet(a', b'))
      | (Error(a'), _) | (_, Error(a')) => Error(a')
      }
  };

  module Bounded_Join_Semilattice: BOUNDED_JOIN_SEMILATTICE_F =
    (Ok: BOUNDED_JOIN_SEMILATTICE, Error: BOUNDED_JOIN_SEMILATTICE) => {
    include Join_Semilattice(Ok, Error);
    let bottom = Error(Error.bottom)
  };

  module Bounded_Meet_Semilattice: BOUNDED_MEET_SEMILATTICE_F =
    (Ok: BOUNDED_MEET_SEMILATTICE, Error: BOUNDED_MEET_SEMILATTICE) => {
    include Meet_Semilattice(Ok, Error);
    let top = Ok(Ok.top)
  };

  module Lattice = (Ok: LATTICE, Error: LATTICE) => {
    include Join_Semilattice(Ok, Error);
    include (Meet_Semilattice(Ok, Error): (module type of Meet_Semilattice(Ok, Error)) with type t := t)
  };

  module Bounded_Lattice = (Ok: BOUNDED_LATTICE, Error: BOUNDED_LATTICE) => {
    include Bounded_Join_Semilattice(Ok, Error);
    include (
      Bounded_Meet_Semilattice(Ok, Error): (module type of Bounded_Meet_Semilattice(Ok, Error)) with type t := t
    )
  };

  module Distributive_Lattice = (Ok: LATTICE, Error: LATTICE) => { include Lattice(Ok, Error) };

  module Bounded_Distributive_Lattice = (Ok: BOUNDED_LATTICE, Error: BOUNDED_LATTICE) => {
    include Bounded_Lattice(Ok, Error)
  };

  module Heyting_Algebra: HEYTING_ALGEBRA_F = (Ok: HEYTING_ALGEBRA, Error: HEYTING_ALGEBRA) => {
    include Quasireflexive_Ord(Ok, Error);
    include (
      Bounded_Distributive_Lattice(Ok, Error):
        (module type of Bounded_Distributive_Lattice(Ok, Error)) with type t := t
    );

    let not = a => switch a {
      | Ok(a') when a' == Ok.top => Error(Error.bottom)
      | Ok(a') when a' == Ok.bottom => Error(Error.top)
      | Error(a') when a' == Error.top => Ok(Ok.bottom) 
      | Error(a') when a' == Error.bottom => Ok(Ok.top) 
      | a' => a'
      };

    let implies = (a, b) => join(not(a), b)
  };

  module Involutive_Heyting_Algebra = (Ok: INVOLUTIVE_HEYTING_ALGEBRA, Error: INVOLUTIVE_HEYTING_ALGEBRA) => {
    include Heyting_Algebra(Ok, Error)
  };

  module Boolean_Algebra = (Ok: BOOLEAN_ALGEBRA, Error: BOOLEAN_ALGEBRA) => { include Heyting_Algebra(Ok, Error) };
};

module Foldable: FOLDABLE_F = (T: TYPE) => {
  type t('a) = Belt.Result.t('a, T.t);

  let fold_left = (f, initial, a) => switch a {
    | Ok(a') => f(initial, a')
    | Error(_) => initial
    }

  and fold_right = (f, initial, a) => switch a {
    | Ok(a') => f(a', initial)
    | Error(_) => initial
    };

  module Fold_Map = (M: MONOID) => {
    let fold_map = (f, a) => switch a { | Ok(a') => f(a') | Error(_) => M.empty }
  };
  module Fold_Map_Plus = (P: PLUS) => {
    let fold_map = (f, a) => switch a { | Ok(a') => f(a') | Error(_) => P.empty }
  };
  module Fold_Map_Any = (M: MONOID_ANY) => {
    let fold_map = (f, a) => switch a { | Ok(a') => f(a') | Error(_) => M.empty }
  };
};

module Bifoldable: BIFOLDABLE with type t('a, 'b) = Belt.Result.t('a, 'b) = {
  type t('a, 'b) = Belt.Result.t('a, 'b);

  let bifold_left = (f, g, initial, a) => switch a {
    | Ok(a') => f(initial, a')
    | Error(a') => g(initial, a')
    }

  and bifold_right = (f, g, initial, a) => switch a {
    | Ok(a') => f(a', initial)
    | Error(a') => g(a', initial)
    };

  module Fold_Map      = (M: MONOID)     => { let fold_map = result };
  module Fold_Map_Any  = (M: MONOID_ANY) => { let fold_map = result };
  module Fold_Map_Plus = (P: PLUS)       => { let fold_map = result };
};

module Traversable: TRAVERSABLE_F = (T: TYPE, A: APPLICATIVE) => {
  module I = Infix.Apply(A);
  module E = Applicative(T);

  type t('a) = Belt.Result.t('a, T.t) and applicative_t('a) = A.t('a);

  include (Functor(T): FUNCTOR with type t('a) := t('a));
  include (Foldable(T): FOLDABLE with type t('a) := t('a));

  let traverse = (f, a) => switch a {
    | Ok(a') => A.map(E.pure, f(a'))
    | Error(a') => A.pure(Error(a'))
    }

  and sequence = a => switch a {
    | Ok(a') => A.map(E.pure, a')
    | Error(a') => A.pure(Error(a'))
    }
};

module Bitraversable = (A: APPLICATIVE) => {
  module I = Infix.Apply(A);

  type t('a, 'b) = Belt.Result.t('a, 'b) and applicative_t('a) = A.t('a);

  include (Bifunctor: BIFUNCTOR with type t('a, 'b) := t('a, 'b));
  include (Bifoldable: BIFOLDABLE with type t('a, 'b) := t('a, 'b));

  let bitraverse = (f, g, a) => switch a {
    | Ok(a') => A.map(x => Ok(x), f(a'))
    | Error(a') => A.map(x => Error(x), g(a'))
    }

  and bisequence = a => switch a {
    | Ok(a') => A.map(x => Ok(x), a')
    | Error(a') => A.map(x => Error(x), a')
    }
};

module Infix = {
  include Infix.Bifunctor(Bifunctor)
};

module Choose = (A: ALT) => {
  let choose: (A.t('a), A.t('b)) => A.t(Belt.Result.t('a, 'b)) =
    (a, b) => A.alt(A.map(x => Ok(x), a), A.map(x => Error(x), b))
};

module Unsafe = {
  let from_ok = a => switch a {
    | Ok(a') => a'
    | _ => Js.Exn.raiseTypeError("You passed in an `Error` value to `from_ok`")
    }

  and from_error = a => switch a {
    | Error(a') => a'
    | _ => Js.Exn.raiseTypeError("You passed in an `Ok` value to `from_error`")
    }
};

let is_ok    = a => result(const(true), const(false))(a)
and is_error = a => result(const(false), const(true))(a)

and note: ('a, option('b)) => Belt.Result.t('a, 'b) =
  default => Option.maybe(~f=x => Ok(x), ~default=Error(default))

and hush: Belt.Result.t('a, 'b) => option('b) =
  e => result(Option.Applicative.pure, const(None))(e)
