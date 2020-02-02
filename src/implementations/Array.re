open Interface;
let (get, length) = ArrayLabels.(get, length);

let zip_with: (('a, 'b) => 'c, array('a), array('b)) => array('c) =
  (f, xs, ys) => {
    let l = length(xs) < length(ys) ? length(xs) : length(ys)
    and result = [||];

    for (i in 0 to l - 1) {
      Js.Array.push(f(get(xs, i), get(ys, i)), result) |> ignore;
    };
    result;
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
  let map = f => ArrayLabels.map(~f);
};

module Apply: APPLY with type t('a) = array('a) = {
  include Functor;
  let apply = (fn_array, a) =>
    ArrayLabels.fold_left(
      ~f=(acc, f) => ArrayLabels.append(acc, map(f, a)),
      ~init=[||],
      fn_array,
    );
};

module Applicative: APPLICATIVE with type t('a) = array('a) = {
  include Apply;
  let pure = a => [|a|];
};

module Monad: MONAD with type t('a) = array('a) = {
  include Applicative;
  let flat_map = (x, f) =>
    ArrayLabels.fold_left(
      ~f=(acc, a) => ArrayLabels.append(acc, f(a)),
      ~init=[||],
      x,
    );
};

module Alt: ALT with type t('a) = array('a) = {
  include Functor;
  let alt = (a, b) => Js.Array.concat(b, a);
};

module Foldable: FOLDABLE with type t('a) = array('a) = {
  type t('a) = array('a);

  let fold_left = (f, init) => ArrayLabels.fold_left(~f, ~init)
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
    | Some((a, next)) => Belt.Array.concat([|a|], unfold(f, next))
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
              A.pure((x, y) => ArrayLabels.append([|x|], y))
              <*> f(acc)
              <*> x,
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
      Js.Array.length(xs) == Js.Array.length(ys)
      && Js.Array.every(((a, b)) => E.eq(a, b), zip(xs, ys));
  };

module Ord: ORD_F =
  (O: ORD) => {
    include Eq(O);
    let compare = (xs, ys) =>
      switch (xs, ys) {
      | _ when Js.Array.length(xs) == Js.Array.length(ys) =>
        Js.Array.reducei(
          (acc, e, index) =>
            acc != `equal_to ? acc : O.compare(e, get(ys, index)),
          `equal_to,
          xs,
        )
      | _ when Js.Array.length(xs) < Js.Array.length(ys) => `less_than
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
    Js.Array.mapi((_, i) => f(Js.Array.sliceFrom(i, xs)), xs);
};

module Infix = {
  include Infix.Monad(Monad);
  include Infix.Extend(Extend);
};
