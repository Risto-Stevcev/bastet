open Interface;

module Magma = (M: MAGMA) => {
  let (<:>) = M.append
};

module Magma_Any = (M: MAGMA_ANY) => {
  let (<:>) = M.append
};

module Functor = (F: FUNCTOR) => {
  let (<$>) = F.map
  and (<#>) = (f, x) => F.map(x, f)
};

module Apply = (A: APPLY) => {
  include Functor(A);
  let (<*>) = A.apply
};

module Monad = (M: MONAD) => {
  include Apply(M);
  let (>>=) = M.flat_map
  and (=<<) = (ma, f) => M.flat_map(f, ma);

  let (>=>) = (f, g, a) => f(a) >>= g
  and (<=<) = (f, g, a) => f =<< g(a)
};

module Alt = (A: ALT) => {
  include Functor(A);
  let (<|>) = A.alt
};

module Alternative = (A: ALTERNATIVE) => {
  include Alt(A);
  include Apply(A)
};

module Semigroupoid = (S: SEMIGROUPOID) => {
  let (<.) = S.compose
  and (>.) = (g, f) => S.compose(f, g)
};

module Eq = (E: EQ) => {
  let (=|=) = E.eq
};

module Ord = (O: ORD) => {
  let ((<||), (||>), (<|=), (>|=)) = {
    module Fn = Ordering(O);
    (Fn.less_than, Fn.greater_than, Fn.less_than_or_equal, Fn.greater_than_or_equal)
  }
};

module Semiring = (S: SEMIRING) => {
  let (|+|) = S.add
  and (|*|) = S.multiply
};

module Ring = (R: RING) => {
  include Semiring(R);
  let (|-|) = R.subtract
};

module Euclidean_Ring = (E: EUCLIDEAN_RING) => {
  include Ring(E);
  let (|/|) = E.divide
  and (|%|) = E.modulo
};

module Extend = (E: EXTEND) => {
  let (<<=) = E.extend
  and (=>>) = (a, f) => E.extend(f, a)
};

module Bifunctor = (B: BIFUNCTOR) => {
  let (<<$>>) = B.bimap
};

module Biapply = (B: BIAPPLY) => {
  include Bifunctor(B);
  let (<<*>>) = B.biapply
};

module Join_Semilattice = (J: JOIN_SEMILATTICE) => {
  let (<||>) = J.join
};

module Meet_Semilattice = (M: MEET_SEMILATTICE) => {
  let (<&&>) = M.meet
};

module Heyting_Algebra = (H: HEYTING_ALGEBRA) => {
  let (-->) = H.implies
};
