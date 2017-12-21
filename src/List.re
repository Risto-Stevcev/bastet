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
