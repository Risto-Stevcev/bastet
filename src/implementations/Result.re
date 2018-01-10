/* Result is the equivalent of Either in Haskell for Ocaml */
open Interface;
open Js.Result;
let (flip, const) = Function.(flip, const);

let result: ('a => 'c, 'b => 'c, Js.Result.t('a, 'b)) => 'c = (f, g, a) => switch (f, g, a) {
  | (f, _, Ok(a')) => f(a')
  | (_, g, Error(a')) => g(a')
  };

module Magma = (T: TYPE, M: MAGMA) => {
  module Result_Magma_: MAGMA with type t = Js.Result.t(M.t, T.t) = {
    type t = Js.Result.t(M.t, T.t);
    let append = (a, b) => switch (a, b) {
      | (Ok(a'), Ok(b')) => Ok(M.append(a', b'))
      | (_, Ok(b')) => Ok(b')
      | (Ok(a'), _) => Ok(a')
      | (Error(a'), _) => Error(a')
      }
  };
  include Result_Magma_
};

module Medial_Magma = (T: TYPE, M: MAGMA) => {
  module Result_Medial_Magma_: MEDIAL_MAGMA with type t = Js.Result.t(M.t, T.t) = {
    include Magma(T, M);
  };
  include Result_Medial_Magma_
};

module Semigroup = (T: TYPE, S: SEMIGROUP) => {
  include Magma(T, S);
};

module Functor = (T: TYPE) => {
  module Result_Functor_: FUNCTOR with type t('a) = Js.Result.t('a, T.t) = {
    type t('a) = Js.Result.t('a, T.t);
    let map = (f, a) => switch a {
    | Ok(r) => Ok(f(r))
    | Error(l) => Error(l)
    }
  };
  include Result_Functor_
};

module Bifunctor: BIFUNCTOR with type t('a, 'b) = Js.Result.t('a, 'b) = {
  type t('a, 'b) = Js.Result.t('a, 'b);
  let bimap = (f, g, a) => switch a {
    | Ok(a') => Ok(f(a'))
    | Error(a') => Error(g(a'))
    }
};

module Apply = (T: TYPE) => {
  module Result_Apply_: APPLY with type t('a) = Js.Result.t('a, T.t) = {
    include Functor(T);
    let apply = (f, a) => switch (f, a) {
      | (Ok(f'), a') => map(f', a')
      | (Error(f'), _) => Error(f')
      }
  };
  include Result_Apply_
};

module Applicative = (T: TYPE) => {
  module Result_Applicative_: APPLICATIVE with type t('a) = Js.Result.t('a, T.t) = {
    include Apply(T);
    let pure = a => Ok(a)
  };
  include Result_Applicative_
};

module Monad = (T: TYPE) => {
  module Result_Monad_: MONAD with type t('a) = Js.Result.t('a, T.t) = {
    include Applicative(T);
    let flat_map = (a, f) => switch a {
      | Ok(a') => f(a')
      | Error(a') => Error(a')
      };
  };
  include Result_Monad_
};

module Alt = (T: TYPE) => {
  module Result_Alt_: ALT with type t('a) = Js.Result.t('a, T.t) = {
    include Functor(T);
    let alt = (a, b) => switch (a, b) {
      | (Error(_), b') => b'
      | (a', _) => a'
      };
  };
  include Result_Alt_
};

module Extend = (T: TYPE) => {
  module Result_Extend_: EXTEND with type t('a) = Js.Result.t('a, T.t) = {
    include Monad(T);
    let extend = (f, a) => switch (f, a) {
      | (_, Error(a')) => Error(a')
      | (f', a') => Ok(f'(a'))
      };
  };
  include Result_Extend_;
};

module Show = (Ok: SHOW, Error: SHOW) => {
  module Result_Show_: SHOW with type t = Js.Result.t(Ok.t, Error.t) = {
    type t = Js.Result.t(Ok.t, Error.t);
    let show = result(Ok.show, Error.show)
  };
  include Result_Show_
};

module Eq = (Ok: EQ, Error: EQ) => {
  module Result_Eq_: EQ with type t = Js.Result.t(Ok.t, Error.t) = {
    type t = Js.Result.t(Ok.t, Error.t);
    let eq = (a, b) => switch (a, b) {
      | (Ok(a'), Ok(b')) => Ok.eq(a', b')
      | (Error(a'), Error(b')) => Error.eq(a', b')
      | _ => false
      }
  };
  include Result_Eq_;
};

module Ord = (Ok: ORD, Error: ORD) => {
  module Result_Ord_: ORD with type t = Js.Result.t(Ok.t, Error.t) = {
    include Eq(Ok, Error);
    let compare = (a, b) => switch (a, b) {
      | (Ok(a'), Ok(b')) => Ok.compare(a', b')
      | (Error(a'), Error(b')) => Error.compare(a', b')
      | (Error(_), Ok(_)) => `less_than
      | (Ok(_), Error(_)) => `greater_than
      }
  };
  include Result_Ord_
};

module Bounded = (Ok: BOUNDED, Error: BOUNDED) => {
  module Result_Bounded_: BOUNDED with type t = Js.Result.t(Ok.t, Error.t) = {
    include Ord(Ok, Error);
    let top = Ok(Ok.top);
    let bottom = Error(Error.bottom);
  };
  include Result_Bounded_
};

module Many_Valued_Logic = {
  /* Many valued logics in general have to relax certain constraints in order to 
   * be heyting or boolean algebras, such as:
   * - The reflexivity conditions of equivalence and implication (may be quasi-reflexive)
   * - The law of excluded middle
   */
  module Quasireflexive_Eq = (Ok: TYPE, Error: TYPE) => {
    module Result_Quasireflexive_Eq_: EQ with type t = Js.Result.t(Ok.t, Error.t) = {
      type t = Js.Result.t(Ok.t, Error.t);
      /* Quasi-reflexive */
      let eq = (a, b) => switch (a, b) {
        | (Ok(_), Ok(_)) | (Error(_), Error(_)) => true 
        | _ => false
        }
    };
    include Result_Quasireflexive_Eq_;
  };

  module Quasireflexive_Ord = (Ok: TYPE, Error: TYPE) => {
    module Result_Quasireflexive_Ord_: ORD with type t = Js.Result.t(Ok.t, Error.t) = {
      include Quasireflexive_Eq(Ok, Error);
      /* Quasi-reflexive */
      let compare = (a, b) => switch (a, b) {
        | (Ok(_), Ok(_)) | (Error(_), Error(_)) => `equal_to 
        | (Error(_), Ok(_)) => `less_than
        | (Ok(_), Error(_)) => `greater_than
        }
    };
    include Result_Quasireflexive_Ord_
  };

  module Join_Semilattice = (Ok: JOIN_SEMILATTICE, Error: JOIN_SEMILATTICE) => {
    module Result_Join_Semilattice_:
      JOIN_SEMILATTICE with type t = Js.Result.t(Ok.t, Error.t) = {
      type t = Js.Result.t(Ok.t, Error.t);
      let join = (a, b) => switch (a, b) {
        | (Ok(a'), Ok(b')) => Ok(Ok.join(a', b'))
        | (Ok(a'), _) | (_, Ok(a')) => Ok(a')
        | (Error(a'), Error(b')) => Error(Error.join(a', b'))
        }
    };
    include Result_Join_Semilattice_
  };

  module Meet_Semilattice = (Ok: MEET_SEMILATTICE, Error: MEET_SEMILATTICE) => {
    module Result_Meet_Semilattice_:
      MEET_SEMILATTICE with type t = Js.Result.t(Ok.t, Error.t) = {
      type t = Js.Result.t(Ok.t, Error.t);
      let meet = (a, b) => switch (a, b) {
        | (Ok(a'), Ok(b')) => Ok(Ok.meet(a', b'))
        | (Error(a'), Error(b')) => Error(Error.meet(a', b'))
        | (Error(a'), _) | (_, Error(a')) => Error(a')
        }
    };
    include Result_Meet_Semilattice_
  };

  module Bounded_Join_Semilattice =
    (Ok: BOUNDED_JOIN_SEMILATTICE, Error: BOUNDED_JOIN_SEMILATTICE) => {
    module Result_Bounded_Join_Semilattice_:
      BOUNDED_JOIN_SEMILATTICE with type t = Js.Result.t(Ok.t, Error.t) = {
      include Join_Semilattice(Ok, Error);
      let bottom = Error(Error.bottom);
    };
    include Result_Bounded_Join_Semilattice_
  };

  module Bounded_Meet_Semilattice =
    (Ok: BOUNDED_MEET_SEMILATTICE, Error: BOUNDED_MEET_SEMILATTICE) => {
    module Result_Bounded_Meet_Semilattice_:
      BOUNDED_MEET_SEMILATTICE with type t = Js.Result.t(Ok.t, Error.t) = {
      include Meet_Semilattice(Ok, Error);
      let top = Ok(Ok.top);
    };
    include Result_Bounded_Meet_Semilattice_
  };

  module Lattice = (Ok: LATTICE, Error: LATTICE) => {
    include Join_Semilattice(Ok, Error);
    include (
      Meet_Semilattice(Ok, Error):
        (module type of Meet_Semilattice(Ok, Error)) with type t := t
    );
  };

  module Bounded_Lattice = (Ok: BOUNDED_LATTICE, Error: BOUNDED_LATTICE) => {
    include Bounded_Join_Semilattice(Ok, Error);
    include (
      Bounded_Meet_Semilattice(Ok, Error):
        (module type of Bounded_Meet_Semilattice(Ok, Error)) with type t := t
    );
  };

  module Distributive_Lattice = (Ok: LATTICE, Error: LATTICE) => {
    include Lattice(Ok, Error);
  };

  module Bounded_Distributive_Lattice = (Ok: BOUNDED_LATTICE, Error: BOUNDED_LATTICE) => {
    include Bounded_Lattice(Ok, Error);
  };

  module Heyting_Algebra = (Ok: HEYTING_ALGEBRA, Error: HEYTING_ALGEBRA) => {
    module Result_Heyting_Algebra_: HEYTING_ALGEBRA with type t = Js.Result.t(Ok.t, Error.t) = {
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
      let implies = (a, b) => join(not(a), b);
    };
    include Result_Heyting_Algebra_
  };

  module Involutive_Heyting_Algebra =
    (Ok: INVOLUTIVE_HEYTING_ALGEBRA, Error: INVOLUTIVE_HEYTING_ALGEBRA) => {
    include Heyting_Algebra(Ok, Error);
  };

  module Boolean_Algebra =
    (Ok: BOOLEAN_ALGEBRA, Error: BOOLEAN_ALGEBRA) => {
    include Heyting_Algebra(Ok, Error);
  };
};

module Foldable = (T: TYPE) => {
  module Result_Foldable_: FOLDABLE with type t('a) = Js.Result.t('a, T.t) = {
    type t('a) = Js.Result.t('a, T.t);
    let fold_left = (f, initial, a) => switch a {
      | Ok(a') => f(initial, a')
      | Error(_) => initial
      };
    let fold_right = (f, initial, a) => switch a {
      | Ok(a') => f(a', initial)
      | Error(_) => initial
      };
    module Fold_Map = (M: MONOID) => {
      let fold_map = (f, a) => switch a {
        | Ok(a') => f(a')
        | Error(_) => M.empty
        }
    };
    module Fold_Map_Plus = (P: PLUS) => {
      let fold_map = (f, a) => switch a {
        | Ok(a') => f(a')
        | Error(_) => P.empty
        }
    };
    module Fold_Map_Any = (M: MONOID_ANY) => {
      let fold_map = (f, a) => switch a {
        | Ok(a') => f(a')
        | Error(_) => M.empty
        }
    };
  };
  include Result_Foldable_
};

module Bifoldable: BIFOLDABLE with type t('a, 'b) = Js.Result.t('a, 'b) = {
  type t('a, 'b) = Js.Result.t('a, 'b);
  let bifold_left = (f, g, initial, a) => switch a {
    | Ok(a') => f(initial, a')
    | Error(a') => g(initial, a')
    };
  let bifold_right = (f, g, initial, a) => switch a {
    | Ok(a') => f(a', initial)
    | Error(a') => g(a', initial)
    };
  module Fold_Map = (M: MONOID) => {
    let fold_map = result
  };
  module Fold_Map_Any = (M: MONOID_ANY) => {
    let fold_map = result
  };
  module Fold_Map_Plus = (P: PLUS) => {
    let fold_map = result
  };
};

module Traversable = (T: TYPE, A: APPLICATIVE) => {
  module I = Infix.Apply(A);
  module E = Applicative(T);
  module Result_Traversable_: TRAVERSABLE with type t('a) = Js.Result.t('a, T.t) = {
    type t('a) = Js.Result.t('a, T.t);
    type applicative_t('a) = A.t('a);
    include (Functor(T): FUNCTOR with type t('a) := t('a));
    include (Foldable(T): FOLDABLE with type t('a) := t('a));

    let traverse = (f, a) => switch a {
      | Ok(a') => A.map(E.pure, f(a'))
      | Error(a') => A.pure(Error(a'))
      };
    let sequence = a => switch a {
      | Ok(a') => A.map(E.pure, a')
      | Error(a') => A.pure(Error(a'))
      };
  };
  include Result_Traversable_;
};

module Bitraversable = (A: APPLICATIVE) => {
  module I = Infix.Apply(A);
  module Result_Bitraversable_: BITRAVERSABLE
    with type applicative_t('a) = A.t('a) and type t('a, 'b) = Js.Result.t('a, 'b) = {
    type t('a, 'b) = Js.Result.t('a, 'b);
    type applicative_t('a) = A.t('a);
    include (Bifunctor: BIFUNCTOR with type t('a, 'b) := t('a, 'b));
    include (Bifoldable: BIFOLDABLE with type t('a, 'b) := t('a, 'b));

    let bitraverse = (f, g, a) => switch a {
      | Ok(a') => A.map(x => Ok(x), f(a'))
      | Error(a') => A.map(x => Error(x), g(a'))
      };
    let bisequence = a => switch a {
      | Ok(a') => A.map(x => Ok(x), a')
      | Error(a') => A.map(x => Error(x), a')
      };
  };
  include Result_Bitraversable_
};

module Infix = {
  include Infix.Bifunctor(Bifunctor);
};

module Choose = (A: ALT) => {
  let choose: (A.t('a), A.t('b)) => A.t(Js.Result.t('a, 'b)) =
    (a, b) => A.alt(A.map(x => Ok(x), a), A.map(x => Error(x), b))
};

module Unsafe = {
  let from_ok = a => switch a {
    | Ok(a') => a'
    | _ => Js.Exn.raiseTypeError("You passed in an `Error` value to `from_ok`")
    };
  let from_error = a => switch a {
    | Error(a') => a'
    | _ => Js.Exn.raiseTypeError("You passed in an `Ok` value to `from_error`")
    };
};

let is_ok = a => result(const(true), const(false))(a);
let is_error = a => result(const(false), const(true))(a);

let note: ('a, option('b)) => Js.Result.t('a, 'b) =
  default => BsAbstract.Option.maybe(~f=x => Ok(x), ~default=Error(default));

let hush: Js.Result.t('a, 'b) => option('b) =
  e => result(BsAbstract.Option.Applicative.pure, const(None))(e);
