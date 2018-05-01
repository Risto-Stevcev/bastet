open Interface;

module type EQ_F          = (E: EQ)          => EQ          with type t = list(E.t);
module type SHOW_F        = (S: SHOW)        => SHOW        with type t = list(S.t);
module type TRAVERSABLE_F = (A: APPLICATIVE) => TRAVERSABLE with type t('a) = list('a)
  and type applicative_t('a) = A.t('a);

module Functor: FUNCTOR with type t('a) = list('a) = {
  type t('a) = list('a);
  let map = (f) => ListLabels.map(~f)
};

module Apply: APPLY with type t('a) = list('a) = {
  include Functor;
  let apply = (fn_array, a) =>
    ListLabels.fold_left(
      ~f=(acc, f) => ListLabels.append(acc, map(f, a)),
      ~init=[],
      fn_array
    )
};

module Applicative: APPLICATIVE with type t('a) = list('a) = {
  include Apply;
  let pure = (a) => [a]
};

module Monad: MONAD with type t('a) = list('a) = {
  include Applicative;
  let flat_map = (x, f) => ListLabels.fold_left(~f=(acc, a) => ListLabels.append(acc, f(a)), ~init=[], x)
};

module Alt: ALT with type t('a) = list('a) = {
  include Functor;
  let alt = ListLabels.append
};

module Plus: PLUS with type t('a) = list('a) = {
  include Alt;
  let empty = []
};

module Alternative: ALTERNATIVE with type t('a) = list('a) = {
  include Applicative;
  include (Plus: PLUS with type t('a) := t('a))
};

module Foldable: FOLDABLE with type t('a) = list('a) = {
  type t('a) = list('a);

  let fold_left  = (f, init) => ListLabels.fold_left(~f, ~init)
  and fold_right = (f, init) => ListLabels.fold_right(~f, ~init);

  module Fold_Map = (M: MONOID) => {
    module D = Default.Fold_Map(M, {
      type t('a) = list('a);
      let (fold_left, fold_right) = (fold_left, fold_right)
    });
    let fold_map = D.fold_map_default_left
  };
  module Fold_Map_Any = (M: MONOID_ANY) => {
    module D = Default.Fold_Map_Any(M, {
      type t('a) = list('a);
      let (fold_left, fold_right) = (fold_left, fold_right)
    });
    let fold_map = D.fold_map_default_left
  };
  module Fold_Map_Plus = (P: PLUS) => {
    module D = Default.Fold_Map_Plus(P, {
      type t('a) = list('a);
      let (fold_left, fold_right) = (fold_left, fold_right)
    });
    let fold_map = D.fold_map_default_left
  }
};

module Traversable: TRAVERSABLE_F = (A: APPLICATIVE) => {
  type t('a) = list('a) and applicative_t('a) = A.t('a);

  include (Functor: FUNCTOR with type t('a) := t('a));
  include (Foldable: FOLDABLE with type t('a) := t('a));

  module I = Infix.Apply(A);

  let traverse = (f) => I.({
    ListLabels.fold_right(
      ~f=(acc, x) => A.pure((y, ys) => [y, ...ys]) <*> f(acc) <*> x,
      ~init=A.pure([])
    )
  });

  module D = Default.Sequence({
    type t('a) = list('a);
    type applicative_t('a) = A.t('a);
    let traverse = traverse
  });

  let sequence = D.sequence_default
};

module Eq: EQ_F = (E: EQ) => {
  type t = list(E.t);
  let eq = (xs, ys) => {
    ListLabels.length(xs) == ListLabels.length(ys) &&
    ListLabels.fold_left(~f=(acc, (a, b)) => acc && E.eq(a, b), ~init=true, ListLabels.combine(xs, ys))
  }
};

module Show: SHOW_F = (S: SHOW) => {
  module F = Functions.Foldable(Foldable);
  module M = F.Monoid(String.Monoid);

  type t = list(S.t);
  let show = xs => "["++ M.intercalate(~separator=", ", Functor.map(S.show, xs)) ++ "]"
};

module Infix = {
  include Infix.Monad(Monad);
  include Infix.Alternative(Alternative);
};
