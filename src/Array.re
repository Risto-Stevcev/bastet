open Interface;

module Semigroup: SEMIGROUP_ANY with type t('a) = array('a) = {
  type t('a) = array('a);
  let append = Js.Array.concat
};


module Monoid: MONOID_ANY with type t('a) = array('a) = {
  include Semigroup;
  let empty = [||]
};


module Functor: FUNCTOR with type t('a) = array('a) = {
  type t('a) = array('a);
  let map = (f) => ArrayLabels.map(~f)
};


module Apply: APPLY with type t('a) = array('a) = {
  include Functor;
  let apply = (fn_array, a) =>
    ArrayLabels.fold_left(
      ~f=(acc, f) => ArrayLabels.append(acc, map(f, a)),
      ~init=[||],
      fn_array
    )
};


module Applicative: APPLICATIVE with type t('a) = array('a) = {
  include Apply;
  let pure = (a) => [|a|];
};


module Monad: MONAD with type t('a) = array('a) = {
  include Applicative;
  let flat_map = (x, f) =>
    ArrayLabels.fold_left(
      ~f=(acc, a) => ArrayLabels.append(acc, f(a)),
      ~init=[||],
      x
    )
};


module Foldable: FOLDABLE with type t('a) = array('a) = {
  type t('a) = array('a);
  let fold_left = (f, init) => ArrayLabels.fold_left(~f, ~init);
  let fold_right = (f, init) => ArrayLabels.fold_right(~f, ~init);

  module Fold_Map = (M: MONOID) => {
     module D = Default.Fold_Map(M, {
      type t('a) = array('a);
      let fold_left = fold_left;
      let fold_right = fold_right;
    });

    let fold_map = D.fold_map_default_left;
  };

  module Fold_Map_Any = (M: MONOID_ANY) => {
    module D = Default.Fold_Map_Any(M, {
      type t('a) = array('a);
      let fold_left = fold_left;
      let fold_right = fold_right;
    });

    let fold_map = D.fold_map_default_left;
  };
};


module type TRAVERSABLE_F = (A: APPLICATIVE) => TRAVERSABLE
  with type applicative_t('a) = A.t('a) and type t('a) = array('a);

module Traversable: TRAVERSABLE_F = (A: APPLICATIVE) => {
  type t('a) = array('a);
  type applicative_t('a) = A.t('a);
  include (Functor: FUNCTOR with type t('a) := t('a));
  include (Foldable: FOLDABLE with type t('a) := t('a));

  module I = Infix.Apply(A);
  let traverse = (f) => I.({
    ArrayLabels.fold_left(
      ~f=(x, acc) => A.pure(Js.Array.append) <*> f(acc) <*> x,
      ~init=A.pure([||])
    )
  });

  module D = Default.Sequence({
    type t('a) = array('a);
    type applicative_t('a) = A.t('a);
    let traverse = traverse;
  });
  let sequence = D.sequence_default;
};
