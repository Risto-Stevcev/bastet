open Interface;

module Semigroup: SEMIGROUP_ANY with type t('a) = list('a) = {
  type t('a) = list('a);
  let append = (@);
};


module Monoid: MONOID_ANY with type t('a) = list('a) = {
  include Semigroup;
  let empty = [];
};


module Functor: FUNCTOR with type t('a) = list('a) = {
  type t('a) = list('a);
  let map = (f) => ListLabels.map(~f);
};


module Apply: APPLY with type t('a) = list('a) = {
  include Functor;
  let apply = (fn_array, a) =>
    ListLabels.fold_left(
      ~f=(acc, f) => ListLabels.append(acc, map(f, a)),
      ~init=[],
      fn_array
    );
};


module Applicative: APPLICATIVE with type t('a) = list('a) = {
  include Apply;
  let pure = (a) => [a];
};


module Monad: MONAD with type t('a) = list('a) = {
  include Applicative;
  let flat_map = (x, f) =>
    ListLabels.fold_left(~f=(acc, a) => ListLabels.append(acc, f(a)), ~init=[], x);
};


module Alt: ALT with type t('a) = list('a) = {
  include Functor;
  let alt = ListLabels.append;
};


module Plus: PLUS with type t('a) = list('a) = {
  include Alt;
  let empty = [];
};


module Alternative: ALTERNATIVE with type t('a) = list('a) = {
  include Applicative;
  include (Plus: PLUS with type t('a) := t('a));
};


module Foldable: FOLDABLE with type t('a) = list('a) = {
  type t('a) = list('a);
  let fold_left = (f, init) => ListLabels.fold_left(~f, ~init);
  let fold_right = (f, init) => ListLabels.fold_right(~f, ~init);

  module Fold_Map = (M: MONOID) => {
     module D = Default.Fold_Map(M, {
      type t('a) = list('a);
      let fold_left = fold_left;
      let fold_right = fold_right;
    });

    let fold_map = D.fold_map_default_left;
  };

  module Fold_Map_Any = (M: MONOID_ANY) => {
    module D = Default.Fold_Map_Any(M, {
      type t('a) = list('a);
      let fold_left = fold_left;
      let fold_right = fold_right;
    });

    let fold_map = D.fold_map_default_left;
  };
};


module type TRAVERSABLE_F = (A: APPLICATIVE) => TRAVERSABLE
  with type applicative_t('a) = A.t('a) and type t('a) = list('a);

module Traversable: TRAVERSABLE_F = (A: APPLICATIVE) => {
  type t('a) = list('a);
  type applicative_t('a) = A.t('a);
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
    let traverse = traverse;
  });
  let sequence = D.sequence_default;
};
