open Interface;

module Magma = (M: MAGMA) => {
  let (<:>) = M.append;
};

module Semigroup = (S: SEMIGROUP) => {
  let (<:>) = S.append;
};

module Semigroup_Any = (S: SEMIGROUP_ANY) => {
  let (<:>) = S.append;
};

module Monoid = (M: MONOID) => {
  include Semigroup(M);
  let (<:>) = M.append;
};

module Monoid_Any = (M: MONOID_ANY) => {
  include Semigroup_Any(M);
  let (<:>) = M.append;
};

module Functor = (F: FUNCTOR) => {
  let (<$>) = F.map;
  let (<#>) = (f, x) => F.map(x, f);
};

module Apply = (A: APPLY) => {
  include Functor(A);
  let (<*>) = A.apply;
};

module Monad = (M: MONAD) => {
  include Apply(M);
  let (>>=) = M.flat_map;
  let (=<<) = (ma, f) => M.flat_map(f, ma);
};

module Alt = (A: ALT) => {
  include Functor(A);
  let (<|>) = A.alt;
};

module Alternative = (A: ALTERNATIVE) => {
  include Alt(A);
  include Apply(A);
};

module Semigroupoid = (S: SEMIGROUPOID) => {
  let (<.) = S.compose;
  let (>.) = (g, f) => S.compose(f, g);
};

module Eq = (E: EQ) => {
  let (=|=) = E.eq;
};

module Ord = (O: ORD) => {
  let ((<||), (||>), (<|=), (>|=)) = {
    module Fn = Ordering(O);
   (Fn.less_than, Fn.greater_than, Fn.less_than_or_equal, Fn.greater_than_or_equal)
  };
};

module Semiring = (S: SEMIRING) => {
  let (|+|) = S.add;
  let (|*|) = S.multiply;
};

module Ring = (R: RING) => {
  include Semiring(R);
  let (|-|) = R.subtract;
};

module Euclidean_Ring = (E: EUCLIDEAN_RING) => {
  include Ring(E);
  let (|/|) = E.divide;
  let (|%|) = E.modulo;
};

module Extend = (E: EXTEND) => {
  let (<<=) = E.extend;
  let (=>>) = (a, f) => E.extend(f, a);
};
