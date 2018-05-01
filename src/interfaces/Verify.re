open Interface;


module Compare = {
  module Medial_Magma = (M: MEDIAL_MAGMA, E: EQ with type t = M.t) => {
    module I = Infix.Magma(M);
    let bicommutativity: (M.t, M.t, M.t, M.t) => bool =
      (a, b, c, d) => I.(E.eq((a <:> b) <:> (c <:> d), ((a <:> c) <:> (b <:> d))))
  };
  module Semigroup = (S: SEMIGROUP, E: EQ with type t = S.t) => {
    module I = Infix.Magma(S);
    let associativity: (S.t, S.t, S.t) => bool =
      (a, b, c) => I.(E.eq(a <:> (b <:> c), a <:> (b <:> c)))
  };
  module Semigroup_Any = (S: SEMIGROUP_ANY, E: EQ1 with type t('a) = S.t('a)) => {
    module I = Infix.Magma_Any(S);
    let associativity: (S.t('a), S.t('a), S.t('a)) => bool =
      (a, b, c) => I.(E.eq(a <:> (b <:> c), a <:> (b <:> c)))
  };
  module Monoid = (M: MONOID, E: EQ with type t = M.t) => {
    module I = Infix.Magma(M);
    let identity: M.t => bool = (a) => I.(E.eq(a <:> M.empty, a) && E.eq(M.empty <:> a, a))
  };
  module Monoid_Any = (M: MONOID_ANY, E: EQ1 with type t('a) = M.t('a)) => {
    module I = Infix.Magma_Any(M);
    let identity: M.t('a) => bool = (a) =>
      I.(E.eq(a <:> M.empty, a) && E.eq(M.empty <:> a, a))
  };
  module Quasigroup = (Q: QUASIGROUP, E: EQ with type t = Q.t) => {
    module I = Infix.Magma(Q);
    let cancellative: (Q.t, Q.t, Q.t) => bool =
      (a, b, c) => I.(!E.eq(a <:> b, (a <:> c)) || E.eq(b, c)) &&
                   I.(!E.eq(b <:> a, (c <:> a)) || E.eq(b, c))
  };
  module Quasigroup_Any = (Q: QUASIGROUP_ANY, E: EQ1 with type t('a) = Q.t('a)) => {
    module I = Infix.Magma_Any(Q);
    let cancellative: (Q.t('a), Q.t('a), Q.t('a)) => bool =
      (a, b, c) => I.(!E.eq(a <:> b, (a <:> c)) || E.eq(b, c)) &&
                   I.(!E.eq(b <:> a, (c <:> a)) || E.eq(b, c))
  };
  module Medial_Quasigroup = (Q: MEDIAL_QUASIGROUP, E: EQ with type t = Q.t) => {
    include Quasigroup(Q, E)
  };
  module Loop = (L: LOOP, E: EQ with type t = L.t) => {
    module I = Infix.Magma(L);
    let identity: L.t => bool =
      (a) => I.(E.eq(a <:> L.empty, a) && E.eq(L.empty <:> a, a))
  };
  module Loop_Any = (L: LOOP_ANY, E: EQ1 with type t('a) = L.t('a)) => {
    module I = Infix.Magma_Any(L);
    let identity: L.t('a) => bool =
      (a) => I.(E.eq(a <:> L.empty, a) && E.eq(L.empty <:> a, a))
  };
  module Group = (G: GROUP, E: EQ with type t = G.t) => {
    module I = Infix.Magma(G);
    /* via Monoid */
    let invertibility: G.t => bool =
      (a) => I.(E.eq(G.inverse(a) <:> a, G.empty) && E.eq(a <:> G.inverse(a), G.empty));
    /* via Loop */
    let associativity: (G.t, G.t, G.t) => bool =
      (a, b, c) => I.(E.eq(a <:> (b <:> c), a <:> (b <:> c)))
  };
  module Group_Any = (G: GROUP_ANY, E: EQ1 with type t('a) = G.t('a)) => {
    module I = Infix.Magma_Any(G);
    /* via Monoid */
    let invertibility: G.t('a) => bool =
      (a) => I.(E.eq(G.inverse(a) <:> a, G.empty) && E.eq(a <:> G.inverse(a), G.empty));
    /* via Loop */
    let associativity: (G.t('a), G.t('a), G.t('a)) => bool =
      (a, b, c) => I.(E.eq(a <:> (b <:> c), a <:> (b <:> c)))
  };
  module Abelian_Group = (A: ABELIAN_GROUP, E: EQ with type t = A.t) => {
    let commutativity: (A.t, A.t) => bool = (a, b) => E.eq(A.append(a, b), A.append(b, a))
  };
  module Abelian_Group_Any = (A: ABELIAN_GROUP_ANY, E: EQ1 with type t('a) = A.t('a)) => {
    let commutativity: (A.t('a), A.t('a)) => bool =
      (a, b) => E.eq(A.append(a, b), A.append(b, a))
  };
  module Functor = (F: FUNCTOR, E: EQ1 with type t('a) = F.t('a)) => {
    let (<.) = Function.Infix.(<.);
    let identity: F.t('a) => bool = (a) => E.eq(F.map(Function.Category.id, a), a);
    let composition: ('b => 'c, 'a => 'b, F.t('a)) => bool =
      (f, g, a) => E.eq(F.map(f <. g, a), (F.map(f) <. F.map(g))(a))
  };
  module Apply = (A: APPLY, E: EQ1 with type t('a) = A.t('a)) => {
    module I = Infix.Apply(A);
    let associative_composition:
      (A.t(('b => 'c)), A.t(('a => 'b)), A.t('a)) => bool = (f, g, h) => I.(
        E.eq(A.map(Function.Semigroupoid.compose, f) <*> g <*> h, f <*> (g <*> h))
      )
  };
  module Applicative = (A: APPLICATIVE, E: EQ1 with type t('a) = A.t('a)) => {
    module I = Infix.Apply(A);
    let identity: A.t('a) => bool = (a) => I.(E.eq(A.pure(Function.Category.id) <*> a, a));
    let homomorphism: ('a => 'b, 'a) => bool =
      (f, x) => I.(E.eq(A.pure(f) <*> A.pure(x), A.pure(f(x))));
    let interchange: (A.t('a => 'b), 'a) => bool =
      (f, x) => I.(E.eq(f <*> A.pure(x), A.pure((f') => f'(x)) <*> f))
  };
  module Monad = (M: MONAD, E: EQ1 with type t('a) = M.t('a)) => {
    module I = Infix.Monad(M);
    let associativity: ('a => M.t('b), 'b => M.t('c), M.t('a)) => bool =
      (f, g, x) => I.(E.eq((x >>= f) >>= g, x >>= (k) => f(k) >>= g));
    let identity: ('a => M.t('b), 'a) => bool =
      (f, x) => I.(E.eq(M.pure(x) >>= f, f(x)) && E.eq(M.pure(x) >>= M.pure, M.pure(x)))
  };
  module Alt = (A: ALT, E: EQ1 with type t('a) = A.t('a)) => {
    module I = Infix.Alt(A);
    let associativity: (A.t('a), A.t('a), A.t('a)) => bool =
      (a, b, c) => I.(E.eq(a <|> (b <|> c), a <|> (b <|> c)));
    let distributivity: ('a => 'b, A.t('a), A.t('a)) => bool =
      (f, a, b) => I.(E.eq(A.map(f, a <|> b), A.map(f, a) <|> A.map(f, b)))
  };
  module Plus = (P: PLUS, E: EQ1 with type t('a) = P.t('a)) => {
    module I = Infix.Alt(P);
    let annihalation: ('a => 'b) => bool = (f) => E.eq(P.map(f, P.empty), P.empty);
    let identity: P.t('a) => bool =
      (a) => I.(E.eq(P.empty <|> a, a) && E.eq(a <|> P.empty, a))
  };
  module Alternative = (A: ALTERNATIVE, E: EQ1 with type t('a) = A.t('a)) => {
    module I = Infix.Alternative(A);
    let distributivity: (A.t('a => 'b), A.t('a => 'b), A.t('a)) => bool =
      (f, g, x) => I.(E.eq((f <|> g) <*> x, (f <*> x) <|> (g <*> x)));
    let annihalation: A.t('a => 'b) => bool = (f) => I.(E.eq(A.empty <*> f, A.empty))
  };
  module Semigroupoid = (S: SEMIGROUPOID, E: EQ2 with type t('a, 'b) = S.t('a, 'b)) => {
    module I = Infix.Semigroupoid(S);
    let associativity: (S.t('c, 'd), S.t('b, 'c), S.t('a, 'b)) => bool =
      (a, b, c) => I.(E.eq(a <. b <. c, a <. (b <. c)))
  };
  module Category = (C: CATEGORY, E: EQ2 with type t('a, 'b) = C.t('a, 'b)) => {
    module I = Infix.Semigroupoid(C);
    let identity: C.t('a, 'b) => bool = a => I.(E.eq(C.id <. a, a) && E.eq(a <. C.id, a))
  };
  module Eq = (E: EQ) => {
    module I = Infix.Eq(E);
    let reflexivity: E.t => bool = (a) => I.(a =|= a);
    let symmetry: (E.t, E.t) => bool = (a, b) => I.(a =|= b == (b =|= a));
    let transitivity: (E.t, E.t, E.t) => bool =
      (a, b, c) => I.(!(a =|= b && (b =|= c)) || (a =|= c))
  };
  module Quasireflexive_Eq = (E: QUASIREFLEXIVE_EQ) => {
    module I = Infix.Eq(E);
    let quasireflexivity: (E.t, E.t) => bool = (a, b) => I.(!(a =|= b) || (a =|= a && b =|= b));
    let symmetry: (E.t, E.t) => bool = (a, b) => I.(a =|= b == (b =|= a));
    let transitivity: (E.t, E.t, E.t) => bool = (a, b, c) => I.(!(a =|= b && (b =|= c)) || (a =|= c))
  };
  module Ord = (E: ORD) => {
    module Ordering_Functions = Infix.Ord(E);
    let ((<|=), (>|=)) = Ordering_Functions.((<|=), (>|=));
    let reflexivity: E.t => bool = (a) => a <|= a;
    let antisymmetry: (E.t, E.t) => bool = (a, b) => !((a <|= b) && (b <|= a)) || (a == b);
    let transitivity: (E.t, E.t, E.t) => bool = (a, b, c) => !((a <|= b) && (b <|= c)) || (a <|= c)
  };
  module Bounded = (B: BOUNDED) => {
    module Ordering_Functions = Infix.Ord(B);
    let (<|=) = Ordering_Functions.((<|=));
    let bounded: B.t => bool = (a) => B.bottom <|= a && (a <|= B.top)
  };
  module Join_Semilattice = (J: JOIN_SEMILATTICE, E: EQ with type t = J.t) => {
    let associativity: (J.t, J.t, J.t) => bool =
      (a, b, c) => E.eq(J.join(a, J.join(b, c)), J.join(J.join(a, b), c));
    let commutativity: (J.t, J.t) => bool = (a, b) => E.eq(J.join(a, b), J.join(b, a));
    let idempotency: J.t => bool = (a) => E.eq(J.join(a, a), a)
  };
  module Meet_Semilattice = (M: MEET_SEMILATTICE, E: EQ with type t = M.t) => {
    let associativity: (M.t, M.t, M.t) => bool =
      (a, b, c) => E.eq(M.meet(a, M.meet(b, c)), M.meet(M.meet(a, b), c));
    let commutativity: (M.t, M.t) => bool = (a, b) => E.eq(M.meet(a, b), M.meet(b, a));
    let idempotency: M.t => bool = (a) => E.eq(M.meet(a, a), a)
  };
  module Bounded_Join_Semilattice = (B: BOUNDED_JOIN_SEMILATTICE, E: EQ with type t = B.t) => {
    let identity: B.t => bool = (a) => B.join(a, B.bottom) == a
  };
  module Bounded_Meet_Semilattice = (B: BOUNDED_MEET_SEMILATTICE, E: EQ with type t = B.t) => {
    let identity: B.t => bool = (a) => B.meet(a, B.top) == a
  };
  module Lattice = (L: LATTICE, E: EQ with type t = L.t) => {
    let absorption: (L.t, L.t) => bool =
      (a, b) => E.eq(L.meet(a, L.join(a, b)), a) && E.eq(L.join(a, L.meet(a, b)), a)
  };
  module Bounded_Lattice = (L: BOUNDED_LATTICE, E: EQ with type t = L.t) => {
    let absorption: (L.t, L.t) => bool =
      (a, b) => L.meet(a, L.join(a, b)) == a && E.eq(L.join(a, L.meet(a, b)), a)
  };
  module Distributive_Lattice = (L: DISTRIBUTIVE_LATTICE, E: EQ with type t = L.t) => {
    let distributivity: (L.t, L.t, L.t) => bool =
      (a, b, c) => E.eq(L.meet(a, L.join(b, c)), L.join(L.meet(a, b), L.meet(a, c)))
  };
  module Bounded_Distributive_Lattice = (L: BOUNDED_DISTRIBUTIVE_LATTICE, E: EQ with type t = L.t) => {
    let distributivity: (L.t, L.t, L.t) => bool =
      (a, b, c) => E.eq(L.meet(a, L.join(b, c)), L.join(L.meet(a, b), L.meet(a, c)))
  };
  module Heyting_Algebra = (H: HEYTING_ALGEBRA, E: EQ with type t = H.t) => {
    module O = Infix.Ord(H);
    let (<|=) = O.(<|=);
    let pseudocomplement: H.t => bool = (a) => E.eq(H.not(a), H.implies(a, H.bottom));
    let relative_pseudocomplement: (H.t, H.t, H.t) => bool =
      (a, b, c) => H.meet(c, a) <|= b == (c <|= H.implies(a, b))
  };
  module Involutive_Heyting_Algebra = (H: HEYTING_ALGEBRA, E: EQ with type t = H.t) => {
    let involution: H.t => bool = (a) => E.eq(H.not(H.not(a)), a)
  };
  module Boolean_Algebra = (B: BOOLEAN_ALGEBRA, E: EQ with type t = B.t) => {
    let excluded_middle: B.t => bool = (a) => E.eq(B.join(a, B.not(a)), B.top)
  };
  module Semiring = (S: SEMIRING, E: EQ with type t = S.t) => {
    module I = Infix.Semiring(S);
    open I;
    let additive_associativity: (S.t, S.t, S.t) => bool =
      (a, b, c) => E.eq((a |+| b) |+| c, a |+| (b |+| c));
    let additive_identity: S.t => bool = (a) => E.eq(S.zero |+| a, a);
    let commutativity: (S.t, S.t) => bool = (a, b) => E.eq(a |+| b, b |+| a);
    let multiplicative_associativity: (S.t, S.t, S.t) => bool =
      (a, b, c) => E.eq((a |*| b) |*| c, a |*| (b |*| c));
    let multiplicative_identity: S.t => bool = (a) => E.eq(S.one |*| a, a);
    let distributivity: (S.t, S.t, S.t) => bool =
      (a, b, c) => E.eq(a |*| (b |+| c), (a |*| b) |+| (a |*| c)) &&
                   E.eq((a |+| b) |*| c, (a |*| c) |+| (b |*| c))
  };
  module Ring = (R: RING, E: EQ with type t = R.t) => {
    module I = Infix.Ring(R);
    let additive_inverse: R.t => bool = (a) => I.(E.eq((R.zero |-| a) |+| a, R.zero))
  };
  module Commutative_Ring = (R: COMMUTATIVE_RING, E: EQ with type t = R.t) => {
    module I = Infix.Ring(R);
    let multiplicative_commutativity: (R.t, R.t) => bool = (a, b) => I.(E.eq(a |*| b, b |*| a))
  };
  module Division_Ring = (R: DIVISION_RING, E: EQ with type t = R.t) => {
    module I = Infix.Ring(R);
    let non_zero_ring: bool = !E.eq(R.zero, R.one);
    let multiplicative_inverse: R.t => bool = (a) => I.( E.eq(R.reciprocal(a) |*| a, R.one) )
  };
  module Euclidean_Ring = (R: EUCLIDEAN_RING, E: EQ with type t = R.t) => {
    module I = Infix.Euclidean_Ring(R);
    let non_zero_ring: bool = !E.eq(R.zero, R.one);
    let integral_domain: (R.t, R.t) => bool =
      (a, b) => I.(!(!E.eq(a, R.zero) && b != R.zero) || !E.eq(a |*| b, R.zero));
    let non_negative_degree: R.t => bool = (a) => E.eq(a, R.zero) || (R.degree(a) >= 0);
    let remainder: (R.t, R.t) => bool = (a, b) => I.({
      if (b != R.zero) {
        let q = a |/| b;
        let r = a |%| b;
        E.eq(a, q |*| b |+| r) &&
        (E.eq(r, R.zero) || (R.degree(r) < R.degree(b)))
      }
      else { true }
    });
    let submultiplicative: (R.t, R.t) => bool = (a, b) => I.(R.degree(a) <= R.degree(a |*| b))
  };
  module Field = (F: FIELD, E: EQ with type t = F.t) => {
    let non_zero_multiplicative_inverse: (F.t, F.t) => bool = (a, b) => E.eq(F.modulo(a, b), F.zero)
  };
  module Invariant = (I: INVARIANT, E: EQ1 with type t('a) = I.t('a)) => {
    let id = Function.Category.id;
    let (<.) = Function.Infix.(<.);
    let identity: I.t('a) => bool = (a) => E.eq(I.imap(id, id, a), a);
    let composition: ('a => 'b, 'b => 'a, 'b => 'a, 'a => 'b, I.t('a)) => bool =
      (f1, f2, g1, g2, a) => E.eq((I.imap(g1, g2) <. I.imap(f1, f2))(a), I.imap(g1 <. f1, f2 <. g2, a))
  };
  module Contravariant = (C: CONTRAVARIANT, E: EQ1 with type t('a) = C.t('a)) => {
    let id = Function.Category.id;
    let (<.) = Function.Infix.(<.);
    let identity: C.t('a) => bool = (a) => E.eq(C.cmap(id, a), a);
    let composition: ('c => 'b, 'b => 'a, C.t('a)) => bool =
      (f, g, a) => E.eq((C.cmap(f) <. C.cmap(g))(a), C.cmap(g <. f, a))
  };
  module Profunctor = (P: PROFUNCTOR, E: EQ2 with type t('a, 'b) = P.t('a, 'b)) => {
    let id = Function.Category.id;
    let ((<.), (>.)) = Function.Infix.((<.), (>.));
    let identity: P.t('a, 'b) => bool = (a) => E.eq(P.dimap(id, id, a), a);
    let composition: ('a => 'b, 'c => 'd, 'e => 'a, 'd => 'f, P.t('b, 'c)) => bool =
      (f1, g1, f2, g2, a) =>
        E.eq(
          (P.dimap(f2, g2) <. P.dimap(f1, g1))(a),
          P.dimap(f2 >. f1, g2 <. g1, a)
        )
  };
  module Monad_Zero = (M: MONAD_ZERO, E: EQ1 with type t('a) = M.t('a)) => {
    let annihalation: ('a => M.t('b)) => bool = f => E.eq(M.flat_map(M.empty, f), M.empty)
  };
  module Monad_Plus = (M: MONAD_PLUS, E: EQ1 with type t('a) = M.t('a)) => {
    let distributivity: ('a => M.t('b), M.t('a), M.t('a)) => bool =
      (f, a, b) => E.eq(M.flat_map(M.alt(a, b), f), M.alt(M.flat_map(a, f), M.flat_map(b, f)))
  };
  module Extend = (X: EXTEND, E: EQ1 with type t('a) = X.t('a)) => {
    let (<.) = Function.Infix.(<.);
    let associativity: (E.t('b) => 'c, E.t('a) => 'b, E.t('a)) => bool =
      (f, g, a) => E.eq((X.extend(f) <. X.extend(g))(a), X.extend(f <. X.extend(g), a))
  };
  module Comonad = (C: COMONAD, E: EQ1 with type t('a) = C.t('a)) => {
    let identity: (C.t('a) => 'a, C.t('a)) => bool =
      (f, a) => E.eq(C.extend(C.extract, a), a) && E.eq(C.extract(C.extend(f, a)), f(a))
  };
  module Bifunctor = (B: BIFUNCTOR, E: EQ2 with type t('a, 'b) = B.t('a, 'b)) => {
    let id = Function.Category.id;
    let (<.) = Function.Infix.(<.);
    let identity: B.t('a, 'b) => bool = (a) => B.bimap(id, id, a) == a;
    let composition: ('b => 'e, 'd => 'f, 'a => 'b, 'c => 'd, B.t('a, 'c)) => bool =
      (f1, g1, f2, g2, a) =>
        E.eq((B.bimap(f1, g1) <. B.bimap(f2, g2))(a), B.bimap(f1 <. f2, g1 <. g2, a))
  };
};


/* Default Verify functors, uses (==) for comparison */
module Medial_Magma = (M: MEDIAL_MAGMA) => {
  include Compare.Medial_Magma(M, {type t = M.t; let eq = (==)})
};
module Semigroup = (S: SEMIGROUP) => {
  include Compare.Semigroup(S, {type t = S.t; let eq = (==)})
};
module Semigroup_Any = (S: SEMIGROUP_ANY) => {
  include Compare.Semigroup_Any(S, {type t('a) = S.t('a); let eq = (==)})
};
module Monoid = (M: MONOID) => {
  include Compare.Monoid(M, {type t = M.t; let eq = (==)})
};
module Monoid_Any = (M: MONOID_ANY) => {
  include Compare.Monoid_Any(M, {type t('a) = M.t('a); let eq = (==)})
};
module Quasigroup = (Q: QUASIGROUP) => {
  include Compare.Quasigroup(Q, {type t = Q.t; let eq = (==)})
};
module Quasigroup_Any = (Q: QUASIGROUP_ANY) => {
  include Compare.Quasigroup_Any(Q, {type t('a) = Q.t('a); let eq = (==)})
};
module Medial_Quasigroup = (Q: MEDIAL_QUASIGROUP) => {
  include Compare.Medial_Quasigroup(Q, {type t = Q.t; let eq = (==)})
};
module Loop = (L: LOOP) => {
  include Compare.Loop(L, {type t = L.t; let eq = (==)})
};
module Loop_Any = (L: LOOP_ANY) => {
  include Compare.Loop_Any(L, {type t('a) = L.t('a); let eq = (==)})
};
module Group = (G: GROUP) => {
  include Compare.Group(G, {type t = G.t; let eq = (==)})
};
module Group_Any = (G: GROUP_ANY) => {
  include Compare.Group_Any(G, {type t('a) = G.t('a); let eq = (==)})
};
module Abelian_Group = (A: ABELIAN_GROUP) => {
  include Compare.Abelian_Group(A, {type t = A.t; let eq = (==)})
};
module Abelian_Group_Any = (A: ABELIAN_GROUP_ANY) => {
  include Compare.Abelian_Group_Any(A, {type t('a) = A.t('a); let eq = (==)})
};
module Functor = (F: FUNCTOR) => {
  include Compare.Functor(F, {type t('a) = F.t('a); let eq = (==)})
};
module Apply = (A: APPLY) => {
  include Compare.Apply(A, {type t('a) = A.t('a); let eq = (==)})
};
module Applicative = (A: APPLICATIVE) => {
  include Compare.Applicative(A, {type t('a) = A.t('a); let eq = (==)})
};
module Monad = (M: MONAD) => {
  include Compare.Monad(M, {type t('a) = M.t('a); let eq = (==)})
};
module Alt = (A: ALT) => {
  include Compare.Alt(A, {type t('a) = A.t('a); let eq = (==)})
};
module Plus = (P: PLUS) => {
  include Compare.Plus(P, {type t('a) = P.t('a); let eq = (==)})
};
module Alternative = (A: ALTERNATIVE) => {
  include Compare.Alternative(A, {type t('a) = A.t('a); let eq = (==)})
};
module Semigroupoid = (S: SEMIGROUPOID) => {
  include Compare.Semigroupoid(S, {type t('a, 'b) = S.t('a, 'b); let eq = (==)})
};
module Category = (C: CATEGORY) => {
  include Compare.Category(C, {type t('a, 'b) = C.t('a, 'b); let eq = (==)})
};
module Eq = (E: EQ) => { include Compare.Eq(E) };
module Ord = (E: ORD) => { include Compare.Ord(E) };
module Bounded = (B: BOUNDED) => { include Compare.Bounded(B) };
module Join_Semilattice = (J: JOIN_SEMILATTICE) => {
  include Compare.Join_Semilattice(J, {type t = J.t; let eq = (==)})
};
module Meet_Semilattice = (M: MEET_SEMILATTICE) => {
  include Compare.Meet_Semilattice(M, {type t = M.t; let eq = (==)})
};
module Bounded_Join_Semilattice = (J: BOUNDED_JOIN_SEMILATTICE) => {
  include Compare.Bounded_Join_Semilattice(J, {type t = J.t; let eq = (==)})
};
module Bounded_Meet_Semilattice = (M: BOUNDED_MEET_SEMILATTICE) => {
  include Compare.Bounded_Meet_Semilattice(M, {type t = M.t; let eq = (==)})
};
module Lattice = (L: LATTICE) => {
  include Compare.Lattice(L, {type t = L.t; let eq = (==)})
};
module Bounded_Lattice = (L: BOUNDED_LATTICE) => {
  include Compare.Bounded_Lattice(L, {type t = L.t; let eq = (==)})
};
module Distributive_Lattice = (L: DISTRIBUTIVE_LATTICE) => {
  include Compare.Distributive_Lattice(L, {type t = L.t; let eq = (==)})
};
module Bounded_Distributive_Lattice = (L: BOUNDED_DISTRIBUTIVE_LATTICE) => {
  include Compare.Bounded_Distributive_Lattice(L, {type t = L.t; let eq = (==)})
};
module Heyting_Algebra = (H: HEYTING_ALGEBRA) => {
  include Compare.Heyting_Algebra(H, {type t = H.t; let eq = (==)})
};
module Involutive_Heyting_Algebra = (H: INVOLUTIVE_HEYTING_ALGEBRA) => {
  include Compare.Involutive_Heyting_Algebra(H, {type t = H.t; let eq = (==)})
};
module Boolean_Algebra = (B: BOOLEAN_ALGEBRA) => {
  include Compare.Boolean_Algebra(B, {type t = B.t; let eq = (==)})
};
module Semiring = (S: SEMIRING) => {
  include Compare.Semiring(S, {type t = S.t; let eq = (==)})
};
module Ring = (R: RING) => {
  include Compare.Ring(R, {type t = R.t; let eq = (==)})
};
module Commutative_Ring = (R: COMMUTATIVE_RING) => {
  include Compare.Commutative_Ring(R, {type t = R.t; let eq = (==)})
};
module Division_Ring = (R: DIVISION_RING) => {
  include Compare.Division_Ring(R, {type t = R.t; let eq = (==)})
};
module Euclidean_Ring = (R: EUCLIDEAN_RING) => {
  include Compare.Euclidean_Ring(R, {type t = R.t; let eq = (==)})
};
module Field = (F: FIELD) => {
  include Compare.Field(F, {type t = F.t; let eq = (==)})
};
module Invariant = (I: INVARIANT) => {
  include Compare.Invariant(I, {type t('a) = I.t('a); let eq = (==)})
};
module Contravariant = (C: CONTRAVARIANT) => {
  include Compare.Contravariant(C, {type t('a) = C.t('a); let eq = (==)})
};
module Profunctor = (P: PROFUNCTOR) => {
  include Compare.Profunctor(P, {type t('a, 'b) = P.t('a, 'b); let eq = (==)})
};
module Monad_Zero = (M: MONAD_ZERO) => {
  include Compare.Monad_Zero(M, {type t('a) = M.t('a); let eq = (==)})
};
module Monad_Plus = (M: MONAD_PLUS) => {
  include Compare.Monad_Plus(M, {type t('a) = M.t('a); let eq = (==)})
};
module Extend = (E: EXTEND) => {
  include Compare.Extend(E, {type t('a) = E.t('a); let eq = (==)})
};
module Comonad = (C: COMONAD) => {
  include Compare.Comonad(C, {type t('a) = C.t('a); let eq = (==)})
};
module Bifunctor = (B: BIFUNCTOR) => {
  include Compare.Bifunctor(B, {type t('a, 'b) = B.t('a, 'b); let eq = (==)})
};
