open Interface;

module Semigroup = (S: SEMIGROUP) => {
  include S;
  let (<:>) = append;
};

module Semigroup_Any = (S: SEMIGROUP_ANY) => {
  include S;
  let (<:>) = append;
};

module Monoid = (M: MONOID) => {
  include M;
  let (<:>) = append;
};

module Monoid_Any = (M: MONOID_ANY) => {
  include M;
  let (<:>) = append;
};

module Functor = (F: FUNCTOR) => {
  include F;
  let (<$>) = map;
  let (<#>) = (f, x) => map(x, f);
};

module Apply = (A: APPLY) => {
  include A;
  let (<*>) = apply;
};

module Monad = (M: MONAD) => {
  include M;
  let (>>=) = flat_map;
  let (=<<) = (ma, f) => flat_map(f, ma);
};

module Alt = (A: ALT) => {
  include A;
  let (<|>) = alt;
};

module Semigroupoid = (S: SEMIGROUPOID) => {
  let (<.) = S.compose;
  let (>.) = (g, f) => S.compose(f, g);
};

module Eq = (E: EQ) => {
  include E;
  let (=|=) = eq;
};

module Ord = (O: ORD) => {
  module Fn = Ordering(O);
  let (<||) = Fn.less_than;
  let (||>) = Fn.greater_than;
  let (<|=) = Fn.less_than_or_equal;
  let (>|=) = Fn.greater_than_or_equal;
};

module Semiring = (S: SEMIRING) => {
  let (|+|) = S.add;
  let (|*|) = S.multiply;
};

module Ring = (R: RING) => {
  module S_ = Semiring(R);
  include S_;
  let (|-|) = R.subtract;
};

module Euclidean_Ring = (E: EUCLIDEAN_RING) => {
  module R_ = Ring(E);
  include R_;
  let (|/|) = E.divide;
  let (|%|) = E.modulo;
};

module Extend = (E: EXTEND) => {
  let (<<=) = E.extend;
  let (=>>) = (a, f) => E.extend(f, a);
};
