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
};

module Apply = (A: APPLY) => {
  include A;
  let (<*>) = apply;
};

module Monad = (M: MONAD) => {
  include M;
  let (>>=) = flat_map;
};

module Alt = (A: ALT) => {
  include A;
  let (<|>) = alt;
};

module Plus = (P: PLUS) => {
  include P;
  let (<|>) = alt;
};

module Semigroupoid = (S: SEMIGROUPOID) => {
  include S;
  let (<<) = compose;
};

module Category = (C: CATEGORY) => {
  include C;
  let (<<) = compose;
};
