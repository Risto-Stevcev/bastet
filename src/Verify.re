module Semigroup = (S: Interface.SEMIGROUP) => {
  module I = Infix.Semigroup(S);
  let associativity: (S.t, S.t, S.t) => bool =
    (a, b, c) => I.(a <:> (b <:> c) == (a <:> (b <:> c)));
};

module Semigroup_Any = (S: Interface.SEMIGROUP_ANY) => {
  module I = Infix.Semigroup_Any(S);
  let associativity: (S.t('a), S.t('a), S.t('a)) => bool =
    (a, b, c) => I.(a <:> (b <:> c) == (a <:> (b <:> c)));
};

module Monoid = (M: Interface.MONOID) => {
  module I = Infix.Monoid(M);
  let neutral: M.t => bool = (a) => I.(a <:> empty == a && empty <:> a == a);
};

module Monoid_Any = (M: Interface.MONOID_ANY) => {
  module I = Infix.Monoid_Any(M);
  let neutral: M.t('a) => bool = (a) => I.(a <:> empty == a && empty <:> a == a);
};

module Functor = (F: Interface.FUNCTOR) => {
  let (<.) = Function.Infix.(<.);
  let identity: F.t('a) => bool = (a) => F.map(Function.Category.id, a) == a;
  let composition: ('b => 'c, 'a => 'b, F.t('a)) => bool =
    (f, g, a) => F.map(f <. g, a) == (F.map(f) <. F.map(g))(a);
};

module Apply = (A: Interface.APPLY) => {
  module I = Infix.Apply(A);
  let associative_composition:
    (A.t(('b => 'c)), A.t(('a => 'b)), A.t('a)) => bool = (f, g, h) => I.({
      A.map(Function.Semigroupoid.compose, f) <*> g <*> h == (f <*> (g <*> h))
    })
};

module Applicative = (A: Interface.APPLICATIVE) => {
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

module Monad = (A: Interface.MONAD) => {
  module I = Infix.Monad(A);

  let associativity: ('a => A.t('b), 'b => A.t('c), A.t('a)) => bool =
    (f, g, x) => I.({
      (x >>= f) >>= g == (x >>= (k) => f(k) >>= g)
    });

  let left_identity: ('a => A.t('b), 'a) => bool =
    (f, x) => I.(pure(x) >>= f == f(x));

  let right_identity: A.t('a) => bool = (x) => I.(x >>= pure == x);
};

module Alt = (A: Interface.ALT) => {
  module I = Infix.Alt(A);
  let associativity: (A.t('a), A.t('a), A.t('a)) => bool =
    (a, b, c) => I.(a <|> (b <|> c) == (a <|> (b <|> c)));
  let distributivity: ('a => 'b, A.t('a), A.t('a)) => bool =
    (f, a, b) => I.(A.map(f, a <|> b) == (A.map(f, a) <|> A.map(f, b)));
};

module Plus = (P: Interface.PLUS) => {
  module I = Infix.Alt(P);
  let annihalation: ('a => 'b) => bool = (f) => P.map(f, P.empty) == P.empty;
  let left_identity: P.t('a) => bool = (a) => I.(P.empty <|> a == a);
  let right_identity: P.t('a) => bool = (a) => I.(a <|> P.empty == a);
};

module Alternative = (A: Interface.ALTERNATIVE) => {
  module I = {
    include Infix.Alt(A);
    include (Infix.Apply(A): (module type of Infix.Apply(A)) with type t('a) := t('a));
  };

  let distributivity: (A.t('a => 'b), A.t('a => 'b), A.t('a)) => bool =
    (f, g, x) => I.((f <|> g) <*> x == ((f <*> x) <|> (g <*> x)));
  let annihalation: A.t('a => 'b) => bool = (f) => I.(A.empty <*> f == A.empty);
};

module Semigroupoid = (S: Interface.SEMIGROUPOID) => {
  module I = Infix.Semigroupoid(S);
  let associativity: (S.t('c, 'd), S.t('b, 'c), S.t('a, 'b)) => bool =
    (a, b, c) => I.(a <. b <. c == (a <. (b <. c)));
};

module Category = (C: Interface.CATEGORY) => {
  module I = Infix.Semigroupoid(C);
  let identity: C.t('a, 'b) => bool = (a) => I.(C.id <. a == a && a <. C.id == a);
};

module Eq = (E: Interface.EQ) => {
  module I = Infix.Eq(E);
  let reflexivity: E.t => bool = (a) => I.(a =|= a);
  let symmetry: (E.t, E.t) => bool = (a, b) => I.(a =|= b == (b =|= a));
  let transitivity: (E.t, E.t, E.t) => bool =
    (a, b, c) => I.(!(a =|= b && (b =|= c)) || (a =|= c));
};

module Ord = (O: Interface.ORD) => {
  module Ordering_Functions = Interface.Ordering(O);
  let ((<|=), (>|=)) = Ordering_Functions.((<|=), (>|=));
  let reflexivity: O.t => bool = (a) => a <|= a;
  let antisymmetry: (O.t, O.t) => bool = (a, b) => !((a <|= b) && (b <|= a)) || (a == b);
  let transitivity: (O.t, O.t, O.t) => bool =
    (a, b, c) => !((a <|= b) && (b <|= c)) || (a <|= c);
};
