open Interface;

let zip_with: (('a, 'b) => 'c, array('a), array('b)) => array('c) =
  (f, xs, ys) => {
    let l =
      Js.Array.length(xs) < Js.Array.length(ys)
        ? Js.Array.length(xs) : Js.Array.length(ys)
    and result = [||];

    for (i in 0 to l - 1) {
      Js.Array.push(f(xs[i], ys[i]), result) |> ignore;
    };
    result;
  };

let zip: (array('a), array('b)) => array('c) =
  (xs, ys) => zip_with((a, b) => (a, b), xs, ys);

module Functor: FUNCTOR with type t('a) = array('a) = {
  type t('a) = array('a);
  let map = f => Js.Array.map(f);
};

module Alt: ALT with type t('a) = array('a) = {
  include Functor;
  let alt = (a, b) => Js.Array.concat(b, a);
};

module Apply: APPLY with type t('a) = array('a) = {
  include Functor;
  let apply = (fn_array, a) =>
    Js.Array.reduce(
      (acc, f) => Js.Array.concat(acc, map(f, a)),
      [||],
      fn_array,
    );
};

module Monad: MONAD with type t('a) = array('a) = {
  include Array.Applicative;
  let flat_map = (x, f) =>
    Js.Array.reduce((acc, a) => Js.Array.concat(acc, f(a)), [||], x);
};

module Foldable: FOLDABLE with type t('a) = array('a) = {
  type t('a) = array('a);

  let fold_left = (f, init) => Js.Array.reduce(f, init)
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
    | Some((a, next)) => Js.Array.concat([|a|], unfold(f, next))
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
              A.pure((x, y) => Js.Array.concat([|x|], y)) <*> f(acc) <*> x,
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

module Eq: Array.EQ_F =
  (E: EQ) => {
    type t = array(E.t);
    let eq = (xs, ys) =>
      Js.Array.every(((a, b)) => E.eq(a, b), zip(xs, ys));
  };

module Ord: Array.ORD_F =
  (O: ORD) => {
    include Eq(O);
    let compare = (xs, ys) =>
      switch (xs, ys) {
      | _ when Js.Array.length(xs) == Js.Array.length(ys) =>
        let index = ref(0);
        ArrayLabels.fold_left(
          ~f=
            (acc, e) => {
              let result = acc != `equal_to ? acc : O.compare(e, ys[index^]);
              index := index^ + 1;
              result;
            },
          ~init=`equal_to,
          xs,
        );
      | _ when Js.Array.length(xs) < Js.Array.length(ys) => `less_than
      | _ => `greater_than
      };
  };
