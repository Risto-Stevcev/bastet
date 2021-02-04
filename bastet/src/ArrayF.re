open Interface;

module type IMPL = {
  let length: array('a) => int;
  let make: (int, 'a) => array('a);
  let append: (array('a), array('a)) => array('a);
  let map: ('a => 'b, array('a)) => array('b);
  let mapi: (('a, int) => 'b, array('a)) => array('b);
  let fold_left: (('a, 'b) => 'a, 'a, array('b)) => 'a;
  let every: ('a => bool, array('a)) => bool;
  let slice: (~start: int, ~end_: int, array('a)) => array('a);
};

module type ARRAY = {
  let zip_with: (('a, 'b) => 'c, array('a), array('b)) => array('c);
  let zip: (array('a), array('b)) => array(('a, 'b));
  module type EQ_F =
    (E: Interface.EQ) => {
                           type t = array(E.t);
                           let eq: (t, t) => bool;
                         };
  module type ORD_F =
    (O: Interface.ORD) =>
     {
      type t = array(O.t);
      let eq: (t, t) => bool;
      let compare: (t, t) => Interface.ordering;
    };
  module type SHOW_F =
    (S: Interface.SHOW) => {
                             type t = array(S.t);
                             let show: t => string;
                           };
  module type TRAVERSABLE_F =
    (A: Interface.APPLICATIVE) =>
     {
      type t('a) = array('a);
      let map: ('a => 'b, t('a)) => t('b);
      let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
      let fold_right: (('b, 'a) => 'a, 'a, t('b)) => 'a;
      module Fold_Map:
        (M: Interface.MONOID) => {let fold_map: ('a => M.t, t('a)) => M.t;};
      module Fold_Map_Any:
        (M: Interface.MONOID_ANY) =>
         {let fold_map: ('a => M.t('b), t('a)) => M.t('b);};
      module Fold_Map_Plus:
        (P: Interface.PLUS) =>
         {let fold_map: ('a => P.t('b), t('a)) => P.t('b);};
      type applicative_t('a) = A.t('a);
      let traverse:
        ('a => applicative_t('b), t('a)) => applicative_t(t('b));
      let sequence: t(applicative_t('a)) => applicative_t(t('a));
    };
  module Functor: {
    type t('a) = array('a);
    let map: ('a => 'b, t('a)) => t('b);
  };
  module Alt: {
    type t('a) = array('a);
    let map: ('a => 'b, t('a)) => t('b);
    let alt: (t('a), t('a)) => t('a);
  };
  module Apply: {
    type t('a) = array('a);
    let map: ('a => 'b, t('a)) => t('b);
    let apply: (t('a => 'b), t('a)) => t('b);
  };
  module Applicative: {
    type t('a) = array('a);
    let map: ('a => 'b, t('a)) => t('b);
    let apply: (t('a => 'b), t('a)) => t('b);
    let pure: 'a => t('a);
  };
  module Monad: {
    type t('a) = array('a);
    let map: ('a => 'b, t('a)) => t('b);
    let apply: (t('a => 'b), t('a)) => t('b);
    let pure: 'a => t('a);
    let flat_map: (t('a), 'a => t('b)) => t('b);
  };
  module Foldable: {
    type t('a) = array('a);
    let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
    let fold_right: (('b, 'a) => 'a, 'a, t('b)) => 'a;
    module Fold_Map:
      (M: Interface.MONOID) => {let fold_map: ('a => M.t, t('a)) => M.t;};
    module Fold_Map_Any:
      (M: Interface.MONOID_ANY) =>
       {let fold_map: ('a => M.t('b), t('a)) => M.t('b);};
    module Fold_Map_Plus:
      (P: Interface.PLUS) =>
       {let fold_map: ('a => P.t('b), t('a)) => P.t('b);};
  };
  module Unfoldable: {
    type t('a) = array('a);
    let unfold: ('a => option(('a, 'a)), 'a) => t('a);
  };
  module Traversable: TRAVERSABLE_F;
  module Eq: EQ_F;
  module Ord: ORD_F;
  module Show: SHOW_F;
  module Invariant: {
    type t('a) = array('a);
    let imap: ('a => 'b, 'b => 'a, t('a)) => t('b);
  };
  module Extend: {
    type t('a) = array('a);
    let map: ('a => 'b, t('a)) => t('b);
    let extend: (t('a) => 'b, t('a)) => t('b);
  };
  module Infix: {
    let (<$>): ('a => 'b, Monad.t('a)) => Monad.t('b);
    let (<@>): (Monad.t('a), 'a => 'b) => Monad.t('b);
    let (<*>): (Monad.t('a => 'b), Monad.t('a)) => Monad.t('b);
    let (>>=): (Monad.t('a), 'a => Monad.t('b)) => Monad.t('b);
    let (=<<): ('a => Monad.t('b), Monad.t('a)) => Monad.t('b);
    let (>=>): ('a => Monad.t('b), 'b => Monad.t('c), 'a) => Monad.t('c);
    let (<=<): ('a => Monad.t('b), 'c => Monad.t('a), 'c) => Monad.t('b);
    let (<<=): (Extend.t('a) => 'b, Extend.t('a)) => Extend.t('b);
    let (=>>): (Extend.t('a), Extend.t('a) => 'b) => Extend.t('b);
  };
};

module Make = (A: IMPL) : ARRAY => {
  let zip_with: (('a, 'b) => 'c, array('a), array('b)) => array('c) =
    (f, xs, ys) => {
      let l = A.length(xs) < A.length(ys) ? A.length(xs) : A.length(ys)
      and index = ref(0)
      and result = ref(None);

      for (i in 0 to l - 1) {
        let value = f(ArrayLabels.get(xs, i), ArrayLabels.get(ys, i));
        switch (result^) {
        | Some(arr) => ArrayLabels.set(arr, index^, value)
        | None => result := Some(A.make(l, value))
        };
        index := index^ + 1;
      };
      switch (result^) {
      | Some(array) => array
      | None => [||]
      };
    };

  let zip: (array('a), array('b)) => array('c) =
    (xs, ys) => zip_with((a, b) => (a, b), xs, ys);

  module type EQ_F = (E: EQ) => EQ with type t = array(E.t);
  module type ORD_F = (O: ORD) => ORD with type t = array(O.t);
  module type SHOW_F = (S: SHOW) => SHOW with type t = array(S.t);
  module type TRAVERSABLE_F =
    (A: APPLICATIVE) =>

      TRAVERSABLE with
        type t('a) = array('a) and type applicative_t('a) = A.t('a);

  module Functor: FUNCTOR with type t('a) = array('a) = {
    type t('a) = array('a);
    let map = A.map;
  };

  module Alt: ALT with type t('a) = array('a) = {
    include Functor;
    let alt = A.append;
  };

  module Apply: APPLY with type t('a) = array('a) = {
    include Functor;
    let apply = (fn_array, a) =>
      A.fold_left((acc, f) => Alt.alt(acc, map(f, a)), [||], fn_array);
  };

  module Applicative: APPLICATIVE with type t('a) = array('a) = {
    include Apply;
    let pure = a => [|a|];
  };

  module Monad: MONAD with type t('a) = array('a) = {
    include Applicative;
    let flat_map = (x, f) =>
      A.fold_left((acc, a) => Alt.alt(acc, f(a)), [||], x);
  };

  module Foldable: FOLDABLE with type t('a) = array('a) = {
    type t('a) = array('a);

    let fold_left = A.fold_left
    and fold_right = (f, init) => ArrayLabels.fold_right(~f, ~init);

    module Fold_Map = (M: MONOID) => {
      module D =
        Default.Fold_Map(
          M,
          {
            type t('a) = array('a);
            let (fold_left, fold_right) = (fold_left, fold_right);
          },
        );
      let fold_map = D.fold_map_default_left;
    };
    module Fold_Map_Any = (M: MONOID_ANY) => {
      module D =
        Default.Fold_Map_Any(
          M,
          {
            type t('a) = array('a);
            let (fold_left, fold_right) = (fold_left, fold_right);
          },
        );
      let fold_map = D.fold_map_default_left;
    };
    module Fold_Map_Plus = (P: PLUS) => {
      module D =
        Default.Fold_Map_Plus(
          P,
          {
            type t('a) = array('a);
            let (fold_left, fold_right) = (fold_left, fold_right);
          },
        );
      let fold_map = D.fold_map_default_left;
    };
  };

  module Unfoldable: UNFOLDABLE with type t('a) = array('a) = {
    type t('a) = array('a);

    let rec unfold = (f, init) =>
      switch (f(init)) {
      | Some((a, next)) => Alt.alt([|a|], unfold(f, next))
      | None => [||]
      };
  };

  module Traversable: TRAVERSABLE_F =
    (A: APPLICATIVE) => {
      type t('a) = array('a)
      and applicative_t('a) = A.t('a);

      include (Functor: FUNCTOR with type t('a) := t('a));
      include (Foldable: FOLDABLE with type t('a) := t('a));

      module I = Infix.Apply(A);

      let traverse = f =>
        I.(
          ArrayLabels.fold_right(
            ~f=
              (acc, x) =>
                A.pure((x, y) => Alt.alt([|x|], y)) <*> f(acc) <*> x,
            ~init=A.pure([||]),
          )
        );

      module D =
        Default.Sequence({
          type t('a) = array('a)
          and applicative_t('a) = A.t('a);
          let traverse = traverse;
        });

      let sequence = D.sequence_default;
    };

  module Eq: EQ_F =
    (E: EQ) => {
      type t = array(E.t);
      let eq = (xs, ys) =>
        A.length(xs) == A.length(ys)
        && A.every(((a, b)) => E.eq(a, b), zip(xs, ys));
    };

  module Ord: ORD_F =
    (O: ORD) => {
      include Eq(O);
      let compare = (xs, ys) =>
        switch (xs, ys) {
        | _ when A.length(xs) == A.length(ys) =>
          let index = ref(0);
          A.fold_left(
            (acc, e) => {
              let result =
                acc != `equal_to
                  ? acc : O.compare(e, ArrayLabels.get(ys, index^));
              index := index^ + 1;
              result;
            },
            `equal_to,
            xs,
          );
        | _ when A.length(xs) < A.length(ys) => `less_than
        | _ => `greater_than
        };
    };

  module Show: SHOW_F =
    (S: SHOW) => {
      module F = Functions.Foldable(Foldable);
      module M = F.Monoid(String.Monoid);

      type t = array(S.t);
      let show = xs =>
        "[" ++ M.intercalate(~separator=", ", Functor.map(S.show, xs)) ++ "]";
    };

  module Invariant: INVARIANT with type t('a) = array('a) = {
    type t('a) = array('a);
    let imap = (f, _) => Functor.map(f);
  };

  module Extend: EXTEND with type t('a) = array('a) = {
    include Functor;
    let extend = (f, xs) =>
      A.mapi((_, i) => f(A.slice(xs, ~start=i, ~end_=A.length(xs))), xs);
  };

  module Infix = {
    include Infix.Monad(Monad);
    include Infix.Extend(Extend);
  };
};
