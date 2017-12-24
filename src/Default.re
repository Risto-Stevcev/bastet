module C = Infix.Category(Function.Category);

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



module type FOLD_MAP = {
  type t('a);
  module Fold_Map_Any: (M: Interface.MONOID_ANY) => {
    let fold_map: ('a => M.t('b), t('a)) => M.t('b);
  };
};

module Fold = (F: FOLD_MAP) => {
  type t('a) = F.t('a);
  module Dual_Endo = Dual.Monoid_Any(Endo.Monoid);
  module Dual_Fold_Map = F.Fold_Map_Any(Dual_Endo.Monoid_Any);
  module Endo_Fold_Map = F.Fold_Map_Any(Endo.Monoid);

  let fold_left_default = (f, init, xs) => C.({
    let Dual.Dual(Endo.Endo(r)) =
      Dual_Fold_Map.fold_map(((x) => Dual.Dual(Endo.Endo(x))) << Function.flip(f), xs);
    r(init);
  });
  let fold_right_default = (f, init, xs) => C.({
    let Endo.Endo(r) = Endo_Fold_Map.fold_map(((x) => Endo.Endo(x)) << f, xs);
    r(init);
  });
};
