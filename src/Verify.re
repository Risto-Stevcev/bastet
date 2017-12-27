open Interface;

module Semigroup = (S: SEMIGROUP) => {
  module I = Infix.Semigroup(S);
  let associativity: (S.t, S.t, S.t) => bool =
    (a, b, c) => I.(a <:> (b <:> c) == (a <:> (b <:> c)));
};

module Semigroup_Any = (S: SEMIGROUP_ANY) => {
  module I = Infix.Semigroup_Any(S);
  let associativity: (S.t('a), S.t('a), S.t('a)) => bool =
    (a, b, c) => I.(a <:> (b <:> c) == (a <:> (b <:> c)));
};

module Monoid = (M: MONOID) => {
  module I = Infix.Monoid(M);
  let neutral: M.t => bool = (a) => I.(a <:> empty == a && empty <:> a == a);
};

module Monoid_Any = (M: MONOID_ANY) => {
  module I = Infix.Monoid_Any(M);
  let neutral: M.t('a) => bool = (a) => I.(a <:> empty == a && empty <:> a == a);
};

module Functor = (F: FUNCTOR) => {
  let (<.) = Function.Infix.(<.);
  let identity: F.t('a) => bool = (a) => F.map(Function.Category.id, a) == a;
  let composition: ('b => 'c, 'a => 'b, F.t('a)) => bool =
    (f, g, a) => F.map(f <. g, a) == (F.map(f) <. F.map(g))(a);
};

module Apply = (A: APPLY) => {
  module I = Infix.Apply(A);
  let associative_composition:
    (A.t(('b => 'c)), A.t(('a => 'b)), A.t('a)) => bool = (f, g, h) => I.({
      A.map(Function.Semigroupoid.compose, f) <*> g <*> h == (f <*> (g <*> h))
    })
};

module Applicative = (A: APPLICATIVE) => {
  module I = Infix.Apply(A);

  let identity: A.t('a) => bool = (a) => I.({
    A.pure(Function.Category.id) <*> a == a
  });
  let homomorphism: ('a => 'b, 'a) => bool = (f, x) => I.({
    A.pure(f) <*> A.pure(x) == A.pure(f(x))
  });
  let interchange: (A.t('a => 'b), 'a) => bool = (f, x) => I.({
    f <*> A.pure(x) == (A.pure((f') => f'(x)) <*> f)
  });
};

module Monad = (A: MONAD) => {
  module I = Infix.Monad(A);

  let associativity: ('a => A.t('b), 'b => A.t('c), A.t('a)) => bool =
    (f, g, x) => I.({
      (x >>= f) >>= g == (x >>= (k) => f(k) >>= g)
    });
  let left_identity: ('a => A.t('b), 'a) => bool =
    (f, x) => I.(pure(x) >>= f == f(x));
  let right_identity: A.t('a) => bool = (x) => I.(x >>= pure == x);
};

module Alt = (A: ALT) => {
  module I = Infix.Alt(A);
  let associativity: (A.t('a), A.t('a), A.t('a)) => bool =
    (a, b, c) => I.(a <|> (b <|> c) == (a <|> (b <|> c)));
  let distributivity: ('a => 'b, A.t('a), A.t('a)) => bool =
    (f, a, b) => I.(A.map(f, a <|> b) == (A.map(f, a) <|> A.map(f, b)));
};

module Plus = (P: PLUS) => {
  module I = Infix.Alt(P);
  let annihalation: ('a => 'b) => bool = (f) => P.map(f, P.empty) == P.empty;
  let left_identity: P.t('a) => bool = (a) => I.(P.empty <|> a == a);
  let right_identity: P.t('a) => bool = (a) => I.(a <|> P.empty == a);
};

module Alternative = (A: ALTERNATIVE) => {
  module I = {
    include Infix.Alt(A);
    include (Infix.Apply(A): (module type of Infix.Apply(A)) with type t('a) := t('a));
  };

  let distributivity: (A.t('a => 'b), A.t('a => 'b), A.t('a)) => bool =
    (f, g, x) => I.((f <|> g) <*> x == ((f <*> x) <|> (g <*> x)));
  let annihalation: A.t('a => 'b) => bool = (f) => I.(A.empty <*> f == A.empty);
};

module Semigroupoid = (S: SEMIGROUPOID) => {
  module I = Infix.Semigroupoid(S);
  let associativity: (S.t('c, 'd), S.t('b, 'c), S.t('a, 'b)) => bool =
    (a, b, c) => I.(a <. b <. c == (a <. (b <. c)));
};

module Category = (C: CATEGORY) => {
  module I = Infix.Semigroupoid(C);
  let identity: C.t('a, 'b) => bool = (a) => I.(C.id <. a == a && a <. C.id == a);
};

module Eq = (E: EQ) => {
  module I = Infix.Eq(E);
  let reflexivity: E.t => bool = (a) => I.(a =|= a);
  let symmetry: (E.t, E.t) => bool = (a, b) => I.(a =|= b == (b =|= a));
  let transitivity: (E.t, E.t, E.t) => bool =
    (a, b, c) => I.(!(a =|= b && (b =|= c)) || (a =|= c));
};

module Ord = (O: ORD) => {
  module Ordering_Functions = Infix.Ord(O);
  let ((<|=), (>|=)) = Ordering_Functions.((<|=), (>|=));
  let reflexivity: O.t => bool = (a) => a <|= a;
  let antisymmetry: (O.t, O.t) => bool = (a, b) => !((a <|= b) && (b <|= a)) || (a == b);
  let transitivity: (O.t, O.t, O.t) => bool =
    (a, b, c) => !((a <|= b) && (b <|= c)) || (a <|= c);
};

module Bounded = (B: BOUNDED) => {
  module Ordering_Functions = Infix.Ord(B);
  let (<|=) = Ordering_Functions.((<|=));
  let bounded: B.t => bool = (a) => B.bottom <|= a && (a <|= B.top);
};

module Join_Semilattice = (J: JOIN_SEMILATTICE) => {
  let associativity: (J.t, J.t, J.t) => bool =
    (a, b, c) => J.join(a, J.join(b, c)) == J.join(J.join(a, b), c);
  let commutativity: (J.t, J.t) => bool =
    (a, b) => J.join(a, b) == J.join(b, a);
  let idempotency: J.t => bool = (a) => J.join(a, a) == a;
};

module Meet_Semilattice = (M: MEET_SEMILATTICE) => {
  let associativity: (M.t, M.t, M.t) => bool =
    (a, b, c) => M.meet(a, M.meet(b, c)) == M.meet(M.meet(a, b), c);
  let commutativity: (M.t, M.t) => bool =
    (a, b) => M.meet(a, b) == M.meet(b, a);
  let idempotency: M.t => bool = (a) => M.meet(a, a) == a;
};

module Bounded_Join_Semilattice = (B: BOUNDED_JOIN_SEMILATTICE) => {
  let neutral: B.t => bool = (a) => B.join(a, B.bottom) == a;
};

module Bounded_Meet_Semilattice = (B: BOUNDED_MEET_SEMILATTICE) => {
  let neutral: B.t => bool = (a) => B.meet(a, B.top) == a;
};

module Lattice = (L: LATTICE) => {
  let absorption: (L.t, L.t) => bool =
    (a, b) => L.meet(a, L.join(a, b)) == a && (L.join(a, L.meet(a, b)) == a);
};

module Bounded_Lattice = (L: BOUNDED_LATTICE) => {
  let absorption: (L.t, L.t) => bool =
    (a, b) => L.meet(a, L.join(a, b)) == a && (L.join(a, L.meet(a, b)) == a);
};

module Distributive_Lattice = (L: DISTRIBUTIVE_LATTICE) => {
  let distributivity: (L.t, L.t, L.t) => bool =
    (a, b, c) => L.meet(a, L.join(b, c)) == L.join(L.meet(a, b), L.meet(a, c));
};

module Bounded_Distributive_Lattice = (L: BOUNDED_DISTRIBUTIVE_LATTICE) => {
  let distributivity: (L.t, L.t, L.t) => bool =
    (a, b, c) => L.meet(a, L.join(b, c)) == L.join(L.meet(a, b), L.meet(a, c));
};

module Heyting_Algebra = (H: HEYTING_ALGEBRA) => {
  module O = Infix.Ord(H);
  let (<|=) = O.(<|=);
  let pseudocomplement: H.t => bool = (a) => H.not(a) == H.implies(a, H.bottom);
  let relative_pseudocomplement: (H.t, H.t, H.t) => bool =
    (a, b, c) => H.meet(c, a) <|= b == (c <|= H.implies(a, b));
};

module Involutive_Heyting_Algebra = (H: HEYTING_ALGEBRA) => {
  let involution: H.t => bool = (a) => H.not(H.not(a)) == a;
};

module Boolean_Algebra = (B: BOOLEAN_ALGEBRA) => {
  let excluded_middle: B.t => bool = (a) => B.join(a, B.not(a)) == B.top;
};

module Semiring = (S: SEMIRING) => {
  module I = Infix.Semiring(S);
  open I;
  let additive_associativity: (S.t, S.t, S.t) => bool =
    (a, b, c) => (a |+| b) |+| c == (a |+| (b |+| c));
  let additive_identity: S.t => bool = (a) => S.zero |+| a == a;
  let commutativity: (S.t, S.t) => bool = (a, b) => a |+| b == (b |+| a);
  let multiplicative_associativity: (S.t, S.t, S.t) => bool =
    (a, b, c) => (a |*| b) |*| c == (a |*| (b |*| c));
  let multiplicative_identity: S.t => bool = (a) => S.one |*| a == a;
  let left_distributivity: (S.t, S.t, S.t) => bool =
    (a, b, c) => a |*| (b |+| c) == ((a |*| b) |+| (a |*| c));
  let right_distributivity: (S.t, S.t, S.t) => bool =
    (a, b, c) => (a |+| b) |*| c == ((a |*| c) |+| (b |*| c));
};

module Ring = (R: RING) => {
  module I = Infix.Ring(R);
  let additive_inverse: R.t => bool = (a) => I.((R.zero |-| a) |+| a == R.zero);
};

module Commutative_Ring = (R: COMMUTATIVE_RING) => {
  module I = Infix.Ring(R);
  let multiplicative_commutativity: (R.t, R.t) => bool =
    (a, b) => I.(a |*| b == (b |*| a));
};

module Division_Ring = (R: DIVISION_RING) => {
  module I = Infix.Ring(R);
  let non_zero_ring: bool = R.zero != R.one;
  let multiplicative_inverse: R.t => bool = (a) => I.({
    R.reciprocal(a) |*| a == R.one
  });
};

module Euclidean_Ring = (R: EUCLIDEAN_RING) => {
  module I = Infix.Euclidean_Ring(R);
  let non_zero_ring: bool = R.zero != R.one;
  let integral_domain: (R.t, R.t) => bool =
    (a, b) => I.(!(a != R.zero && b != R.zero) || (a |*| b != R.zero));
  let non_negative_degree: R.t => bool = (a) => !(a != R.zero) || (R.degree(a) >= 0);
  let remainder: (R.t, R.t) => bool = (a, b) => I.({
    if (b != R.zero) {
      let q = a |/| b;
      let r = a |%| b;
      a == (q |*| b |+| r) &&
      (r == R.zero || (R.degree(r) < R.degree(b)))
    }
    else { true }
  });
  let submultiplicative: (R.t, R.t) => bool =
    (a, b) => I.(R.degree(a) <= R.degree(a |*| b));
};

module Field = (F: FIELD) => {
  let non_zero_multiplicative_inverse: (F.t, F.t) => bool =
    (a, b) => F.modulo(a, b) == F.zero;
};

module Invariant = (I: INVARIANT) => {
  let id = Function.Category.id;
  let (<.) = Function.Infix.(<.);
  let identity: I.t('a) => bool = (a) => I.imap(id, id, a) == a;
  let composition: ('a => 'b, 'b => 'a, 'b => 'a, 'a => 'b, I.t('a)) => bool =
    (f1, f2, g1, g2, a) => {
      (I.imap(g1, g2) <. I.imap(f1, f2))(a) ==
      I.imap(g1 <. f1, f2 <. g2, a)
    }
};

module Contravariant = (C: CONTRAVARIANT) => {
  let id = Function.Category.id;
  let (<.) = Function.Infix.(<.);
  let identity: C.t('a) => bool = (a) => C.cmap(id, a) == a;
  let composition: ('c => 'b, 'b => 'a, C.t('a)) => bool =
    (f, g, a) => (C.cmap(f) <. C.cmap(g))(a) == C.cmap(g <. f, a);
};

module Profunctor = (P: PROFUNCTOR) => {
  let id = Function.Category.id;
  let ((<.), (>.)) = Function.Infix.((<.), (>.));
  let identity: P.t('a, 'b) => bool = (a) => P.dimap(id, id, a) == a;
  let composition: ('a => 'b, 'c => 'd, 'e => 'a, 'd => 'f, P.t('b, 'c)) => bool =
    (f1, g1, f2, g2, a) => {
      (P.dimap(f2, g2) <. P.dimap(f1, g1))(a) ==
      P.dimap(f2 >. f1, g2 <. g1, a);
    }
};

module Monad_Zero = (M: MONAD_ZERO) => {
  let annihalation: ('a => M.t('b)) => bool = (f) => M.flat_map(M.empty, f) == M.empty;
};

module Monad_Plus = (M: MONAD_PLUS) => {
  let distributivity: ('a => M.t('b), M.t('a), M.t('a)) => bool =
    (f, a, b) => M.flat_map(M.alt(a, b), f) == M.alt(M.flat_map(a, f), M.flat_map(b, f));
};

module Extend = (E: EXTEND) => {
  let (<.) = Function.Infix.(<.);
  let associativity: (E.t('b) => 'c, E.t('a) => 'b, E.t('a)) => bool =
    (f, g, a) => (E.extend(f) <. E.extend(g))(a) == E.extend(f <. E.extend(g), a);
};

module Comonad = (C: COMONAD) => {
  let left_identity: C.t('a) => bool = (a) => C.extend(C.extract, a) == a;
  let right_identity: (C.t('a) => 'a, C.t('a)) => bool =
    (f, a) => C.extract(C.extend(f, a)) == f(a);
};

module Bifunctor = (B: BIFUNCTOR) => {
  let id = Function.Category.id;
  let (<.) = Function.Infix.(<.);
  let identity: B.t('a, 'b) => bool = (a) => B.bimap(id, id, a) == a;
  let composition: ('b => 'e, 'd => 'f, 'a => 'b, 'c => 'd, B.t('a, 'c)) => bool =
    (f1, g1, f2, g2, a) => {
      (B.bimap(f1, g1) <. B.bimap(f2, g2))(a) ==
      B.bimap(f1 <. f2, g1 <. g2, a)
    };
};
