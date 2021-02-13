open Interface

module Compare = struct
  module Medial_Magma (M : MEDIAL_MAGMA) (E : EQ with type t = M.t) = struct
    module I = Infix.Magma (M)

    let bicommutativity =
      (fun a b c d ->
         let open I in
         E.eq (a <:> b <:> (c <:> d)) (a <:> c <:> (b <:> d))
        : M.t -> M.t -> M.t -> M.t -> bool)
  end

  module Semigroup (S : SEMIGROUP) (E : EQ with type t = S.t) = struct
    module I = Infix.Magma (S)

    let associativity =
      (fun a b c ->
         let open I in
         E.eq (a <:> (b <:> c)) (a <:> (b <:> c))
        : S.t -> S.t -> S.t -> bool)
  end

  module Semigroup_Any (S : SEMIGROUP_ANY) (E : EQ1 with type 'a t = 'a S.t) = struct
    module I = Infix.Magma_Any (S)

    let associativity =
      (fun a b c ->
         let open I in
         E.eq (a <:> (b <:> c)) (a <:> (b <:> c))
        : 'a S.t -> 'a S.t -> 'a S.t -> bool)
  end

  module Monoid (M : MONOID) (E : EQ with type t = M.t) = struct
    module I = Infix.Magma (M)

    let identity =
      (fun a ->
         let open I in
         E.eq (a <:> M.empty) a && E.eq (M.empty <:> a) a
        : M.t -> bool)
  end

  module Monoid_Any (M : MONOID_ANY) (E : EQ1 with type 'a t = 'a M.t) = struct
    module I = Infix.Magma_Any (M)

    let identity =
      (fun a ->
         let open I in
         E.eq (a <:> M.empty) a && E.eq (M.empty <:> a) a
        : 'a M.t -> bool)
  end

  module Quasigroup (Q : QUASIGROUP) (E : EQ with type t = Q.t) = struct
    module I = Infix.Magma (Q)

    let cancellative =
      (fun a b c ->
         (let open I in
         (not (E.eq (a <:> b) (a <:> c))) || E.eq b c)
         &&
         let open I in
         (not (E.eq (b <:> a) (c <:> a))) || E.eq b c
        : Q.t -> Q.t -> Q.t -> bool)
  end

  module Quasigroup_Any (Q : QUASIGROUP_ANY) (E : EQ1 with type 'a t = 'a Q.t) = struct
    module I = Infix.Magma_Any (Q)

    let cancellative =
      (fun a b c ->
         (let open I in
         (not (E.eq (a <:> b) (a <:> c))) || E.eq b c)
         &&
         let open I in
         (not (E.eq (b <:> a) (c <:> a))) || E.eq b c
        : 'a Q.t -> 'a Q.t -> 'a Q.t -> bool)
  end

  module Medial_Quasigroup (Q : MEDIAL_QUASIGROUP) (E : EQ with type t = Q.t) = struct
    include Quasigroup (Q) (E)
  end

  module Loop (L : LOOP) (E : EQ with type t = L.t) = struct
    module I = Infix.Magma (L)

    let identity =
      (fun a ->
         let open I in
         E.eq (a <:> L.empty) a && E.eq (L.empty <:> a) a
        : L.t -> bool)
  end

  module Loop_Any (L : LOOP_ANY) (E : EQ1 with type 'a t = 'a L.t) = struct
    module I = Infix.Magma_Any (L)

    let identity =
      (fun a ->
         let open I in
         E.eq (a <:> L.empty) a && E.eq (L.empty <:> a) a
        : 'a L.t -> bool)
  end

  module Group (G : GROUP) (E : EQ with type t = G.t) = struct
    module I = Infix.Magma (G)

    let invertibility =
      (fun a ->
         let open I in
         E.eq (G.inverse a <:> a) G.empty && E.eq (a <:> G.inverse a) G.empty
        : G.t -> bool)
      [@@ocaml.doc " via {!Interface.MONOID} "]

    let associativity =
      (fun a b c ->
         let open I in
         E.eq (a <:> (b <:> c)) (a <:> (b <:> c))
        : G.t -> G.t -> G.t -> bool)
      [@@ocaml.doc " via {!Interface.LOOP} "]
  end

  module Group_Any (G : GROUP_ANY) (E : EQ1 with type 'a t = 'a G.t) = struct
    module I = Infix.Magma_Any (G)

    let invertibility =
      (fun a ->
         let open I in
         E.eq (G.inverse a <:> a) G.empty && E.eq (a <:> G.inverse a) G.empty
        : 'a G.t -> bool)
      [@@ocaml.doc " via {!Interface.MONOID} "]

    let associativity =
      (fun a b c ->
         let open I in
         E.eq (a <:> (b <:> c)) (a <:> (b <:> c))
        : 'a G.t -> 'a G.t -> 'a G.t -> bool)
      [@@ocaml.doc " via {!Interface.LOOP} "]
  end

  module Abelian_Group (A : ABELIAN_GROUP) (E : EQ with type t = A.t) = struct
    let commutativity = (fun a b -> E.eq (A.append a b) (A.append b a) : A.t -> A.t -> bool)
  end

  module Abelian_Group_Any (A : ABELIAN_GROUP_ANY) (E : EQ1 with type 'a t = 'a A.t) = struct
    let commutativity = (fun a b -> E.eq (A.append a b) (A.append b a) : 'a A.t -> 'a A.t -> bool)
  end

  module Functor (F : FUNCTOR) (E : EQ1 with type 'a t = 'a F.t) = struct
    let ( <. ) = Function.Infix.( <. )

    let identity = (fun a -> E.eq (F.map Function.Category.id a) a : 'a F.t -> bool)

    let composition =
      (fun f g a -> E.eq (F.map (f <. g) a) ((F.map f <. F.map g) a)
        : ('b -> 'c) -> ('a -> 'b) -> 'a F.t -> bool)
  end

  module Apply (A : APPLY) (E : EQ1 with type 'a t = 'a A.t) = struct
    module I = Infix.Apply (A)

    let associative_composition =
      (fun f g h ->
         let open I in
         E.eq (A.map Function.Semigroupoid.compose f <*> g <*> h) (f <*> (g <*> h))
        : ('b -> 'c) A.t -> ('a -> 'b) A.t -> 'a A.t -> bool)
  end

  module Applicative (A : APPLICATIVE) (E : EQ1 with type 'a t = 'a A.t) = struct
    module I = Infix.Apply (A)

    let identity =
      (fun a ->
         let open I in
         E.eq (A.pure Function.Category.id <*> a) a
        : 'a A.t -> bool)

    let homomorphism =
      (fun f x ->
         let open I in
         E.eq (A.pure f <*> A.pure x) (A.pure (f x))
        : ('a -> 'b) -> 'a -> bool)

    let interchange =
      (fun f x ->
         let open I in
         E.eq (f <*> A.pure x) (A.pure (fun f' -> f' x) <*> f)
        : ('a -> 'b) A.t -> 'a -> bool)
  end

  module Monad (M : MONAD) (E : EQ1 with type 'a t = 'a M.t) = struct
    module I = Infix.Monad (M)

    let associativity =
      (fun f g x ->
         let open I in
         E.eq (x >>= f >>= g) (x >>= fun k -> f k >>= g)
        : ('a -> 'b M.t) -> ('b -> 'c M.t) -> 'a M.t -> bool)

    let identity =
      (fun f x ->
         let open I in
         E.eq (M.pure x >>= f) (f x) && E.eq (M.pure x >>= M.pure) (M.pure x)
        : ('a -> 'b M.t) -> 'a -> bool)
  end

  module Alt (A : ALT) (E : EQ1 with type 'a t = 'a A.t) = struct
    module I = Infix.Alt (A)

    let associativity =
      (fun a b c ->
         let open I in
         E.eq (a <|> (b <|> c)) (a <|> (b <|> c))
        : 'a A.t -> 'a A.t -> 'a A.t -> bool)

    let distributivity =
      (fun f a b ->
         let open I in
         E.eq (A.map f (a <|> b)) (A.map f a <|> A.map f b)
        : ('a -> 'b) -> 'a A.t -> 'a A.t -> bool)
  end

  module Plus (P : PLUS) (E : EQ1 with type 'a t = 'a P.t) = struct
    module I = Infix.Alt (P)

    let annihalation = (fun f -> E.eq (P.map f P.empty) P.empty : ('a -> 'b) -> bool)

    let identity =
      (fun a ->
         let open I in
         E.eq (P.empty <|> a) a && E.eq (a <|> P.empty) a
        : 'a P.t -> bool)
  end

  module Alternative (A : ALTERNATIVE) (E : EQ1 with type 'a t = 'a A.t) = struct
    module I = Infix.Alternative (A)

    let distributivity =
      (fun f g x ->
         let open I in
         E.eq (f <|> g <*> x) (f <*> x <|> (g <*> x))
        : ('a -> 'b) A.t -> ('a -> 'b) A.t -> 'a A.t -> bool)

    let annihalation =
      (fun f ->
         let open I in
         E.eq (A.empty <*> f) A.empty
        : ('a -> 'b) A.t -> bool)
  end

  module Semigroupoid (S : SEMIGROUPOID) (E : EQ2 with type ('a, 'b) t = ('a, 'b) S.t) = struct
    module I = Infix.Semigroupoid (S)

    let associativity =
      (fun a b c ->
         let open I in
         E.eq (a <. b <. c) (a <. (b <. c))
        : ('c, 'd) S.t -> ('b, 'c) S.t -> ('a, 'b) S.t -> bool)
  end

  module Category (C : CATEGORY) (E : EQ2 with type ('a, 'b) t = ('a, 'b) C.t) = struct
    module I = Infix.Semigroupoid (C)

    let identity =
      (fun a ->
         let open I in
         E.eq (C.id <. a) a && E.eq (a <. C.id) a
        : ('a, 'b) C.t -> bool)
  end

  module Eq (E : EQ) = struct
    module I = Infix.Eq (E)

    let reflexivity =
      (fun a ->
         let open I in
         a =|= a
        : E.t -> bool)

    let symmetry =
      (fun a b ->
         let open I in
         a =|= b = (b =|= a)
        : E.t -> E.t -> bool)

    let transitivity =
      (fun a b c ->
         let open I in
         (not (a =|= b && b =|= c)) || a =|= c
        : E.t -> E.t -> E.t -> bool)
  end

  module Quasireflexive_Eq (E : QUASIREFLEXIVE_EQ) = struct
    module I = Infix.Eq (E)

    let quasireflexivity =
      (fun a b ->
         let open I in
         (not (a =|= b)) || (a =|= a && b =|= b)
        : E.t -> E.t -> bool)

    let symmetry =
      (fun a b ->
         let open I in
         a =|= b = (b =|= a)
        : E.t -> E.t -> bool)

    let transitivity =
      (fun a b c ->
         let open I in
         (not (a =|= b && b =|= c)) || a =|= c
        : E.t -> E.t -> E.t -> bool)
  end

  module Ord (E : ORD) = struct
    module Ordering_Functions = Infix.Ord (E)

    let ( <|= ), ( >|= ) =
      let open Ordering_Functions in
      ( <|= ), ( >|= )

    let reflexivity = (fun a -> a <|= a : E.t -> bool)

    let antisymmetry = (fun a b -> (not (a <|= b && b <|= a)) || a = b : E.t -> E.t -> bool)

    let transitivity =
      (fun a b c -> (not (a <|= b && b <|= c)) || a <|= c : E.t -> E.t -> E.t -> bool)
  end

  module Bounded (B : BOUNDED) = struct
    module Ordering_Functions = Infix.Ord (B)

    let ( <|= ) =
      let open Ordering_Functions in
      ( <|= )

    let bounded = (fun a -> B.bottom <|= a && a <|= B.top : B.t -> bool)
  end

  module Join_Semilattice (J : JOIN_SEMILATTICE) (E : EQ with type t = J.t) = struct
    let associativity =
      (fun a b c -> E.eq (J.join a (J.join b c)) (J.join (J.join a b) c)
        : J.t -> J.t -> J.t -> bool)

    let commutativity = (fun a b -> E.eq (J.join a b) (J.join b a) : J.t -> J.t -> bool)

    let idempotency = (fun a -> E.eq (J.join a a) a : J.t -> bool)
  end

  module Meet_Semilattice (M : MEET_SEMILATTICE) (E : EQ with type t = M.t) = struct
    let associativity =
      (fun a b c -> E.eq (M.meet a (M.meet b c)) (M.meet (M.meet a b) c)
        : M.t -> M.t -> M.t -> bool)

    let commutativity = (fun a b -> E.eq (M.meet a b) (M.meet b a) : M.t -> M.t -> bool)

    let idempotency = (fun a -> E.eq (M.meet a a) a : M.t -> bool)
  end

  module Bounded_Join_Semilattice (B : BOUNDED_JOIN_SEMILATTICE) (E : EQ with type t = B.t) = struct
    let identity = (fun a -> B.join a B.bottom = a : B.t -> bool)
  end

  module Bounded_Meet_Semilattice (B : BOUNDED_MEET_SEMILATTICE) (E : EQ with type t = B.t) = struct
    let identity = (fun a -> B.meet a B.top = a : B.t -> bool)
  end

  module Lattice (L : LATTICE) (E : EQ with type t = L.t) = struct
    let absorption =
      (fun a b -> E.eq (L.meet a (L.join a b)) a && E.eq (L.join a (L.meet a b)) a
        : L.t -> L.t -> bool)
  end

  module Bounded_Lattice (L : BOUNDED_LATTICE) (E : EQ with type t = L.t) = struct
    let absorption =
      (fun a b -> L.meet a (L.join a b) = a && E.eq (L.join a (L.meet a b)) a : L.t -> L.t -> bool)
  end

  module Distributive_Lattice (L : DISTRIBUTIVE_LATTICE) (E : EQ with type t = L.t) = struct
    let distributivity =
      (fun a b c -> E.eq (L.meet a (L.join b c)) (L.join (L.meet a b) (L.meet a c))
        : L.t -> L.t -> L.t -> bool)
  end

  module Bounded_Distributive_Lattice (L : BOUNDED_DISTRIBUTIVE_LATTICE) (E : EQ with type t = L.t) =
  struct
    let distributivity =
      (fun a b c -> E.eq (L.meet a (L.join b c)) (L.join (L.meet a b) (L.meet a c))
        : L.t -> L.t -> L.t -> bool)
  end

  module Heyting_Algebra (H : HEYTING_ALGEBRA) (E : EQ with type t = H.t) = struct
    module O = Infix.Ord (H)

    let ( <|= ) = O.( <|= )

    let pseudocomplement = (fun a -> E.eq (H.not a) (H.implies a H.bottom) : H.t -> bool)

    let relative_pseudocomplement =
      (fun a b c -> H.meet c a <|= b = (c <|= H.implies a b) : H.t -> H.t -> H.t -> bool)
  end

  module Involutive_Heyting_Algebra (H : HEYTING_ALGEBRA) (E : EQ with type t = H.t) = struct
    let involution = (fun a -> E.eq (H.not (H.not a)) a : H.t -> bool)
  end

  module Boolean_Algebra (B : BOOLEAN_ALGEBRA) (E : EQ with type t = B.t) = struct
    let excluded_middle = (fun a -> E.eq (B.join a (B.not a)) B.top : B.t -> bool)
  end

  module Semiring (S : SEMIRING) (E : EQ with type t = S.t) = struct
    module I = Infix.Semiring (S)
    open I

    let additive_associativity =
      (fun a b c -> E.eq (a |+| b |+| c) (a |+| (b |+| c)) : S.t -> S.t -> S.t -> bool)

    let additive_identity = (fun a -> E.eq (S.zero |+| a) a : S.t -> bool)

    let commutativity = (fun a b -> E.eq (a |+| b) (b |+| a) : S.t -> S.t -> bool)

    let multiplicative_associativity =
      (fun a b c -> E.eq (a |*| b |*| c) (a |*| (b |*| c)) : S.t -> S.t -> S.t -> bool)

    let multiplicative_identity = (fun a -> E.eq (S.one |*| a) a : S.t -> bool)

    let distributivity =
      (fun a b c ->
         E.eq (a |*| (b |+| c)) (a |*| b |+| (a |*| c))
         && E.eq (a |+| b |*| c) (a |*| c |+| (b |*| c))
        : S.t -> S.t -> S.t -> bool)
  end

  module Ring (R : RING) (E : EQ with type t = R.t) = struct
    module I = Infix.Ring (R)

    let additive_inverse =
      (fun a ->
         let open I in
         E.eq (R.zero |-| a |+| a) R.zero
        : R.t -> bool)
  end

  module Commutative_Ring (R : COMMUTATIVE_RING) (E : EQ with type t = R.t) = struct
    module I = Infix.Ring (R)

    let multiplicative_commutativity =
      (fun a b ->
         let open I in
         E.eq (a |*| b) (b |*| a)
        : R.t -> R.t -> bool)
  end

  module Division_Ring (R : DIVISION_RING) (E : EQ with type t = R.t) = struct
    module I = Infix.Ring (R)

    let non_zero_ring = (not (E.eq R.zero R.one) : bool)

    let multiplicative_inverse =
      (fun a ->
         let open I in
         E.eq (R.reciprocal a |*| a) R.one
        : R.t -> bool)
  end

  module Euclidean_Ring (R : EUCLIDEAN_RING) (E : EQ with type t = R.t) = struct
    module I = Infix.Euclidean_Ring (R)

    let non_zero_ring = (not (E.eq R.zero R.one) : bool)

    let integral_domain =
      (fun a b ->
         let open I in
         (not ((not (E.eq a R.zero)) && b <> R.zero)) || not (E.eq (a |*| b) R.zero)
        : R.t -> R.t -> bool)

    let non_negative_degree = (fun a -> E.eq a R.zero || R.degree a >= 0 : R.t -> bool)

    let remainder =
      (fun a b ->
         let open I in
         if b <> R.zero
         then
           let q = a |/| b in
           let r = a |%| b in
           E.eq a (q |*| b |+| r) && (E.eq r R.zero || R.degree r < R.degree b)
         else true
        : R.t -> R.t -> bool)

    let submultiplicative =
      (fun a b ->
         let open I in
         R.degree a <= R.degree (a |*| b)
        : R.t -> R.t -> bool)
  end

  module Field (F : FIELD) (E : EQ with type t = F.t) = struct
    let non_zero_multiplicative_inverse =
      (fun a b -> E.eq (F.modulo a b) F.zero : F.t -> F.t -> bool)
  end

  module Invariant (I : INVARIANT) (E : EQ1 with type 'a t = 'a I.t) = struct
    let id = Function.Category.id

    let ( <. ) = Function.Infix.( <. )

    let identity = (fun a -> E.eq (I.imap id id a) a : 'a I.t -> bool)

    let composition =
      (fun f1 f2 g1 g2 a -> E.eq ((I.imap g1 g2 <. I.imap f1 f2) a) (I.imap (g1 <. f1) (f2 <. g2) a)
        : ('a -> 'b) -> ('b -> 'a) -> ('b -> 'a) -> ('a -> 'b) -> 'a I.t -> bool)
  end

  module Contravariant (C : CONTRAVARIANT) (E : EQ1 with type 'a t = 'a C.t) = struct
    let id = Function.Category.id

    let ( <. ) = Function.Infix.( <. )

    let identity = (fun a -> E.eq (C.cmap id a) a : 'a C.t -> bool)

    let composition =
      (fun f g a -> E.eq ((C.cmap f <. C.cmap g) a) (C.cmap (g <. f) a)
        : ('c -> 'b) -> ('b -> 'a) -> 'a C.t -> bool)
  end

  module Profunctor (P : PROFUNCTOR) (E : EQ2 with type ('a, 'b) t = ('a, 'b) P.t) = struct
    let id = Function.Category.id

    let ( <. ), ( >. ) =
      let open Function.Infix in
      ( <. ), ( >. )

    let identity = (fun a -> E.eq (P.dimap id id a) a : ('a, 'b) P.t -> bool)

    let composition =
      (fun f1 g1 f2 g2 a ->
         E.eq ((P.dimap f2 g2 <. P.dimap f1 g1) a) (P.dimap (f2 >. f1) (g2 <. g1) a)
        : ('a -> 'b) -> ('c -> 'd) -> ('e -> 'a) -> ('d -> 'f) -> ('b, 'c) P.t -> bool)
  end

  module Monad_Zero (M : MONAD_ZERO) (E : EQ1 with type 'a t = 'a M.t) = struct
    let annihalation = (fun f -> E.eq (M.flat_map M.empty f) M.empty : ('a -> 'b M.t) -> bool)
  end

  module Monad_Plus (M : MONAD_PLUS) (E : EQ1 with type 'a t = 'a M.t) = struct
    let distributivity =
      (fun f a b -> E.eq (M.flat_map (M.alt a b) f) (M.alt (M.flat_map a f) (M.flat_map b f))
        : ('a -> 'b M.t) -> 'a M.t -> 'a M.t -> bool)
  end

  module Extend (X : EXTEND) (E : EQ1 with type 'a t = 'a X.t) = struct
    let ( <. ) = Function.Infix.( <. )

    let associativity =
      (fun f g a -> E.eq ((X.extend f <. X.extend g) a) (X.extend (f <. X.extend g) a)
        : ('b E.t -> 'c) -> ('a E.t -> 'b) -> 'a E.t -> bool)
  end

  module Comonad (C : COMONAD) (E : EQ1 with type 'a t = 'a C.t) = struct
    let identity =
      (fun f a -> E.eq (C.extend C.extract a) a && E.eq (C.extract (C.extend f a)) (f a)
        : ('a C.t -> 'a) -> 'a C.t -> bool)
  end

  module Bifunctor (B : BIFUNCTOR) (E : EQ2 with type ('a, 'b) t = ('a, 'b) B.t) = struct
    let id = Function.Category.id

    let ( <. ) = Function.Infix.( <. )

    let identity = (fun a -> E.eq (B.bimap id id a) a : ('a, 'b) B.t -> bool)

    let composition =
      (fun f1 g1 f2 g2 a ->
         E.eq ((B.bimap f1 g1 <. B.bimap f2 g2) a) (B.bimap (f1 <. f2) (g1 <. g2) a)
        : ('b -> 'e) -> ('d -> 'f) -> ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) B.t -> bool)
  end

  module Bicontravariant (B : BICONTRAVARIANT) (E : EQ2 with type ('a, 'b) t = ('a, 'b) B.t) =
  struct
    let id = Function.Category.id

    let ( <. ) = Function.Infix.( <. )

    let identity = (fun a -> E.eq (B.bicmap id id a) a : ('a, 'b) B.t -> bool)

    let composition =
      (fun f1 g1 f2 g2 a ->
         E.eq ((B.bicmap f1 g1 <. B.bicmap f2 g2) a) (B.bicmap (f2 <. f1) (g2 <. g1) a)
        : ('e -> 'b) -> ('f -> 'd) -> ('b -> 'a) -> ('d -> 'c) -> ('a, 'c) B.t -> bool)
  end
end
[@@ocaml.doc " Provides functors to verify that instances are lawful. "]

module Medial_Magma (M : MEDIAL_MAGMA) = struct
  include
    Compare.Medial_Magma
      (M)
      (struct
        type t = M.t

        let eq = ( = )
      end)
end
[@@ocaml.doc " Default {!Verify} functors. Uses {!Pervasives.( == )} for comparison. "]

module Semigroup (S : SEMIGROUP) = struct
  include
    Compare.Semigroup
      (S)
      (struct
        type t = S.t

        let eq = ( = )
      end)
end

module Semigroup_Any (S : SEMIGROUP_ANY) = struct
  include
    Compare.Semigroup_Any
      (S)
      (struct
        type 'a t = 'a S.t

        let eq = ( = )
      end)
end

module Monoid (M : MONOID) = struct
  include
    Compare.Monoid
      (M)
      (struct
        type t = M.t

        let eq = ( = )
      end)
end

module Monoid_Any (M : MONOID_ANY) = struct
  include
    Compare.Monoid_Any
      (M)
      (struct
        type 'a t = 'a M.t

        let eq = ( = )
      end)
end

module Quasigroup (Q : QUASIGROUP) = struct
  include
    Compare.Quasigroup
      (Q)
      (struct
        type t = Q.t

        let eq = ( = )
      end)
end

module Quasigroup_Any (Q : QUASIGROUP_ANY) = struct
  include
    Compare.Quasigroup_Any
      (Q)
      (struct
        type 'a t = 'a Q.t

        let eq = ( = )
      end)
end

module Medial_Quasigroup (Q : MEDIAL_QUASIGROUP) = struct
  include
    Compare.Medial_Quasigroup
      (Q)
      (struct
        type t = Q.t

        let eq = ( = )
      end)
end

module Loop (L : LOOP) = struct
  include
    Compare.Loop
      (L)
      (struct
        type t = L.t

        let eq = ( = )
      end)
end

module Loop_Any (L : LOOP_ANY) = struct
  include
    Compare.Loop_Any
      (L)
      (struct
        type 'a t = 'a L.t

        let eq = ( = )
      end)
end

module Group (G : GROUP) = struct
  include
    Compare.Group
      (G)
      (struct
        type t = G.t

        let eq = ( = )
      end)
end

module Group_Any (G : GROUP_ANY) = struct
  include
    Compare.Group_Any
      (G)
      (struct
        type 'a t = 'a G.t

        let eq = ( = )
      end)
end

module Abelian_Group (A : ABELIAN_GROUP) = struct
  include
    Compare.Abelian_Group
      (A)
      (struct
        type t = A.t

        let eq = ( = )
      end)
end

module Abelian_Group_Any (A : ABELIAN_GROUP_ANY) = struct
  include
    Compare.Abelian_Group_Any
      (A)
      (struct
        type 'a t = 'a A.t

        let eq = ( = )
      end)
end

module Functor (F : FUNCTOR) = struct
  include
    Compare.Functor
      (F)
      (struct
        type 'a t = 'a F.t

        let eq = ( = )
      end)
end

module Apply (A : APPLY) = struct
  include
    Compare.Apply
      (A)
      (struct
        type 'a t = 'a A.t

        let eq = ( = )
      end)
end

module Applicative (A : APPLICATIVE) = struct
  include
    Compare.Applicative
      (A)
      (struct
        type 'a t = 'a A.t

        let eq = ( = )
      end)
end

module Monad (M : MONAD) = struct
  include
    Compare.Monad
      (M)
      (struct
        type 'a t = 'a M.t

        let eq = ( = )
      end)
end

module Alt (A : ALT) = struct
  include
    Compare.Alt
      (A)
      (struct
        type 'a t = 'a A.t

        let eq = ( = )
      end)
end

module Plus (P : PLUS) = struct
  include
    Compare.Plus
      (P)
      (struct
        type 'a t = 'a P.t

        let eq = ( = )
      end)
end

module Alternative (A : ALTERNATIVE) = struct
  include
    Compare.Alternative
      (A)
      (struct
        type 'a t = 'a A.t

        let eq = ( = )
      end)
end

module Semigroupoid (S : SEMIGROUPOID) = struct
  include
    Compare.Semigroupoid
      (S)
      (struct
        type ('a, 'b) t = ('a, 'b) S.t

        let eq = ( = )
      end)
end

module Category (C : CATEGORY) = struct
  include
    Compare.Category
      (C)
      (struct
        type ('a, 'b) t = ('a, 'b) C.t

        let eq = ( = )
      end)
end

module Eq (E : EQ) = struct
  include Compare.Eq (E)
end

module Ord (E : ORD) = struct
  include Compare.Ord (E)
end

module Bounded (B : BOUNDED) = struct
  include Compare.Bounded (B)
end

module Join_Semilattice (J : JOIN_SEMILATTICE) = struct
  include
    Compare.Join_Semilattice
      (J)
      (struct
        type t = J.t

        let eq = ( = )
      end)
end

module Meet_Semilattice (M : MEET_SEMILATTICE) = struct
  include
    Compare.Meet_Semilattice
      (M)
      (struct
        type t = M.t

        let eq = ( = )
      end)
end

module Bounded_Join_Semilattice (J : BOUNDED_JOIN_SEMILATTICE) = struct
  include
    Compare.Bounded_Join_Semilattice
      (J)
      (struct
        type t = J.t

        let eq = ( = )
      end)
end

module Bounded_Meet_Semilattice (M : BOUNDED_MEET_SEMILATTICE) = struct
  include
    Compare.Bounded_Meet_Semilattice
      (M)
      (struct
        type t = M.t

        let eq = ( = )
      end)
end

module Lattice (L : LATTICE) = struct
  include
    Compare.Lattice
      (L)
      (struct
        type t = L.t

        let eq = ( = )
      end)
end

module Bounded_Lattice (L : BOUNDED_LATTICE) = struct
  include
    Compare.Bounded_Lattice
      (L)
      (struct
        type t = L.t

        let eq = ( = )
      end)
end

module Distributive_Lattice (L : DISTRIBUTIVE_LATTICE) = struct
  include
    Compare.Distributive_Lattice
      (L)
      (struct
        type t = L.t

        let eq = ( = )
      end)
end

module Bounded_Distributive_Lattice (L : BOUNDED_DISTRIBUTIVE_LATTICE) = struct
  include
    Compare.Bounded_Distributive_Lattice
      (L)
      (struct
        type t = L.t

        let eq = ( = )
      end)
end

module Heyting_Algebra (H : HEYTING_ALGEBRA) = struct
  include
    Compare.Heyting_Algebra
      (H)
      (struct
        type t = H.t

        let eq = ( = )
      end)
end

module Involutive_Heyting_Algebra (H : INVOLUTIVE_HEYTING_ALGEBRA) = struct
  include
    Compare.Involutive_Heyting_Algebra
      (H)
      (struct
        type t = H.t

        let eq = ( = )
      end)
end

module Boolean_Algebra (B : BOOLEAN_ALGEBRA) = struct
  include
    Compare.Boolean_Algebra
      (B)
      (struct
        type t = B.t

        let eq = ( = )
      end)
end

module Semiring (S : SEMIRING) = struct
  include
    Compare.Semiring
      (S)
      (struct
        type t = S.t

        let eq = ( = )
      end)
end

module Ring (R : RING) = struct
  include
    Compare.Ring
      (R)
      (struct
        type t = R.t

        let eq = ( = )
      end)
end

module Commutative_Ring (R : COMMUTATIVE_RING) = struct
  include
    Compare.Commutative_Ring
      (R)
      (struct
        type t = R.t

        let eq = ( = )
      end)
end

module Division_Ring (R : DIVISION_RING) = struct
  include
    Compare.Division_Ring
      (R)
      (struct
        type t = R.t

        let eq = ( = )
      end)
end

module Euclidean_Ring (R : EUCLIDEAN_RING) = struct
  include
    Compare.Euclidean_Ring
      (R)
      (struct
        type t = R.t

        let eq = ( = )
      end)
end

module Field (F : FIELD) = struct
  include
    Compare.Field
      (F)
      (struct
        type t = F.t

        let eq = ( = )
      end)
end

module Invariant (I : INVARIANT) = struct
  include
    Compare.Invariant
      (I)
      (struct
        type 'a t = 'a I.t

        let eq = ( = )
      end)
end

module Contravariant (C : CONTRAVARIANT) = struct
  include
    Compare.Contravariant
      (C)
      (struct
        type 'a t = 'a C.t

        let eq = ( = )
      end)
end

module Profunctor (P : PROFUNCTOR) = struct
  include
    Compare.Profunctor
      (P)
      (struct
        type ('a, 'b) t = ('a, 'b) P.t

        let eq = ( = )
      end)
end

module Monad_Zero (M : MONAD_ZERO) = struct
  include
    Compare.Monad_Zero
      (M)
      (struct
        type 'a t = 'a M.t

        let eq = ( = )
      end)
end

module Monad_Plus (M : MONAD_PLUS) = struct
  include
    Compare.Monad_Plus
      (M)
      (struct
        type 'a t = 'a M.t

        let eq = ( = )
      end)
end

module Extend (E : EXTEND) = struct
  include
    Compare.Extend
      (E)
      (struct
        type 'a t = 'a E.t

        let eq = ( = )
      end)
end

module Comonad (C : COMONAD) = struct
  include
    Compare.Comonad
      (C)
      (struct
        type 'a t = 'a C.t

        let eq = ( = )
      end)
end

module Bifunctor (B : BIFUNCTOR) = struct
  include
    Compare.Bifunctor
      (B)
      (struct
        type ('a, 'b) t = ('a, 'b) B.t

        let eq = ( = )
      end)
end

module Bicontravariant (B : BICONTRAVARIANT) = struct
  include
    Compare.Bicontravariant
      (B)
      (struct
        type ('a, 'b) t = ('a, 'b) B.t

        let eq = ( = )
      end)
end
