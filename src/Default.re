module type FOLD = {
  type t('a);
  let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
  let fold_right: (('b, 'a) => 'a, 'a, t('b)) => 'a;
};

module Fold_Map = (M: Interface.MONOID, F: FOLD) => {
  module I = Infix.Monoid(M);

  let fold_map_default_left =
    (f, x) => I.(F.fold_left((acc, x) => acc <:> f(x), M.empty, x));

  let fold_map_default_right =
    (f, x) => I.(F.fold_right((x, acc) => f(x) <:> acc, M.empty, x));
};

module Fold_Map_Any = (M: Interface.MONOID_ANY, F: FOLD) => {
  module I = Infix.Monoid_Any(M);

  let fold_map_default_left =
    (f, x) => I.(F.fold_left((acc, x) => acc <:> f(x), M.empty, x));

  let fold_map_default_right =
    (f, x) => I.(F.fold_right((x, acc) => f(x) <:> acc, M.empty, x));
};
