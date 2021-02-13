module type TYPE = sig
  type t
end

module type MAGMA = sig
  type t

  val append : t -> t -> t
end

module type MEDIAL_MAGMA = sig
  include MAGMA
end

module type MAGMA_ANY = sig
  type 'a t

  val append : 'a t -> 'a t -> 'a t
end

module type SEMIGROUP = sig
  include MAGMA
end

module type SEMIGROUP_ANY = sig
  include MAGMA_ANY
end

module type MONOID = sig
  include SEMIGROUP

  val empty : t
end

module type MONOID_ANY = sig
  include SEMIGROUP_ANY

  val empty : 'a t
end

module type QUASIGROUP = sig
  include MAGMA
end

module type MEDIAL_QUASIGROUP = sig
  include MEDIAL_MAGMA
end

module type QUASIGROUP_ANY = sig
  include MAGMA_ANY
end

module type LOOP = sig
  include QUASIGROUP

  val empty : t
end

module type LOOP_ANY = sig
  include QUASIGROUP_ANY

  val empty : 'a t
end

module type GROUP = sig
  include LOOP

  include MONOID with type t := t

  val inverse : t -> t
end

module type GROUP_ANY = sig
  include LOOP_ANY

  include MONOID_ANY with type 'a t := 'a t

  val inverse : 'a t -> 'a t
end

module type GROUP_LOOP = functor (G : GROUP) -> LOOP

module type GROUP_LOOP_ANY = functor (G : GROUP_ANY) -> LOOP_ANY

module type ABELIAN_GROUP = sig
  include GROUP
end

module type ABELIAN_GROUP_ANY = sig
  include GROUP_ANY
end

module type FUNCTOR = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type APPLY = sig
  include FUNCTOR

  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

module type APPLICATIVE = sig
  include APPLY

  val pure : 'a -> 'a t
end

module type MONAD = sig
  include APPLICATIVE

  val flat_map : 'a t -> ('a -> 'b t) -> 'b t
end

module type ALT = sig
  include FUNCTOR

  val alt : 'a t -> 'a t -> 'a t
end

module type PLUS = sig
  include ALT

  val empty : 'a t
end

module type ALTERNATIVE = sig
  include APPLICATIVE

  include PLUS with type 'a t := 'a t
end

module type FOLDABLE = sig
  type 'a t

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  val fold_right : ('b -> 'a -> 'a) -> 'a -> 'b t -> 'a

  module Fold_Map : functor (M : MONOID) -> sig
    val fold_map : ('a -> M.t) -> 'a t -> M.t
  end

  module Fold_Map_Any : functor (M : MONOID_ANY) -> sig
    val fold_map : ('a -> 'b M.t) -> 'a t -> 'b M.t
  end

  module Fold_Map_Plus : functor (P : PLUS) -> sig
    val fold_map : ('a -> 'b P.t) -> 'a t -> 'b P.t
  end
end

module type UNFOLDABLE = sig
  type 'a t

  val unfold : ('a -> ('a * 'a) option) -> 'a -> 'a t
end

module type TRAVERSABLE = sig
  include FUNCTOR

  include FOLDABLE with type 'a t := 'a t

  type 'a applicative_t

  val traverse : ('a -> 'b applicative_t) -> 'a t -> 'b t applicative_t

  val sequence : 'a applicative_t t -> 'a t applicative_t
end

module type TRAVERSABLE_F = functor (A : APPLICATIVE) ->
  TRAVERSABLE with type 'a applicative_t = 'a A.t

module type SEMIGROUPOID = sig
  type ('a, 'b) t

  val compose : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
end

module type CATEGORY = sig
  include SEMIGROUPOID

  val id : ('a, 'a) t
end

module type EQ = sig
  type t

  val eq : t -> t -> bool
end

module type QUASIREFLEXIVE_EQ = sig
  include EQ
end

module type EQ1 = sig
  type 'a t

  val eq : 'a t -> 'a t -> bool
end

module type EQ2 = sig
  type ('a, 'b) t

  val eq : ('a, 'b) t -> ('a, 'b) t -> bool
end

type ordering =
  [ `less_than
  | `equal_to
  | `greater_than ]

let invert ordering =
  match ordering with
  | `less_than -> `greater_than
  | `greater_than -> `less_than
  | `equal_to -> `equal_to

let int_to_ordering x =
  match x < 0 with
  | true -> `less_than
  | false -> (
      match x = 0 with
      | true -> `equal_to
      | false -> `greater_than)

let unsafe_compare a b =
  match a < b with
  | true -> `less_than
  | false -> (
      match a = b with
      | true -> `equal_to
      | false -> `greater_than)

module type ORD = sig
  include EQ

  val compare : t -> t -> ordering
end

module Ordering (O : ORD) = struct
  let less_than = (fun a b -> O.compare a b = `less_than : O.t -> O.t -> bool)

  and greater_than = (fun a b -> O.compare a b = `greater_than : O.t -> O.t -> bool)

  and less_than_or_equal = (fun a b -> O.compare a b <> `greater_than : O.t -> O.t -> bool)

  and greater_than_or_equal = (fun a b -> O.compare a b <> `less_than : O.t -> O.t -> bool)
end

module type BOUNDED = sig
  include ORD

  val top : t

  val bottom : t
end

module type JOIN_SEMILATTICE = sig
  type t

  val join : t -> t -> t
end

module type MEET_SEMILATTICE = sig
  type t

  val meet : t -> t -> t
end

module type BOUNDED_JOIN_SEMILATTICE = sig
  include JOIN_SEMILATTICE

  val bottom : t
end

module type BOUNDED_MEET_SEMILATTICE = sig
  include MEET_SEMILATTICE

  val top : t
end

module type LATTICE = sig
  include JOIN_SEMILATTICE

  include MEET_SEMILATTICE with type t := t
end

module type BOUNDED_LATTICE = sig
  include BOUNDED_JOIN_SEMILATTICE

  include BOUNDED_MEET_SEMILATTICE with type t := t
end

module type DISTRIBUTIVE_LATTICE = sig
  include LATTICE
end

module type BOUNDED_DISTRIBUTIVE_LATTICE = sig
  include BOUNDED_LATTICE
end

module type HEYTING_ALGEBRA = sig
  include ORD

  include BOUNDED_DISTRIBUTIVE_LATTICE with type t := t

  val not : t -> t

  val implies : t -> t -> t
end

module type INVOLUTIVE_HEYTING_ALGEBRA = sig
  include HEYTING_ALGEBRA
end

module type BOOLEAN_ALGEBRA = sig
  include HEYTING_ALGEBRA
end

module type SHOW = sig
  type t

  val show : t -> string
end

module type SEMIRING = sig
  type t

  val add : t -> t -> t

  val zero : t

  val multiply : t -> t -> t

  val one : t
end

module type RING = sig
  include SEMIRING

  val subtract : t -> t -> t
end

module type COMMUTATIVE_RING = sig
  include RING
end

module type DIVISION_RING = sig
  include RING

  val reciprocal : t -> t
end

module type EUCLIDEAN_RING = sig
  include COMMUTATIVE_RING

  val degree : t -> int

  val divide : t -> t -> t

  val modulo : t -> t -> t
end

module type FIELD = sig
  include EUCLIDEAN_RING

  include DIVISION_RING with type t := t
end

module type INVARIANT = sig
  type 'a t

  val imap : ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t
end

module type CONTRAVARIANT = sig
  type 'a t

  val cmap : ('b -> 'a) -> 'a t -> 'b t
end

module type PROFUNCTOR = sig
  type ('a, 'b) t

  val dimap : ('a -> 'b) -> ('c -> 'd) -> ('b, 'c) t -> ('a, 'd) t
end

module type MONAD_ZERO = sig
  include MONAD

  include ALTERNATIVE with type 'a t := 'a t
end

module type MONAD_PLUS = sig
  include MONAD_ZERO
end

module type EXTEND = sig
  include FUNCTOR

  val extend : ('a t -> 'b) -> 'a t -> 'b t
end

module type COMONAD = sig
  include EXTEND

  val extract : 'a t -> 'a
end

module type BIFUNCTOR = sig
  type ('a, 'b) t

  val bimap : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
end

module type BIAPPLY = sig
  include BIFUNCTOR

  val biapply : ('a -> 'b, 'c -> 'd) t -> ('a, 'c) t -> ('b, 'd) t
end

module type BIAPPLICATIVE = sig
  include BIAPPLY

  val bipure : 'a -> 'b -> ('a, 'b) t
end

module type BIFOLDABLE = sig
  type ('a, 'b) t

  val bifold_left : ('c -> 'a -> 'c) -> ('c -> 'b -> 'c) -> 'c -> ('a, 'b) t -> 'c

  val bifold_right : ('a -> 'c -> 'c) -> ('b -> 'c -> 'c) -> 'c -> ('a, 'b) t -> 'c

  module Fold_Map : functor (M : MONOID) -> sig
    val fold_map : ('a -> M.t) -> ('b -> M.t) -> ('a, 'b) t -> M.t
  end

  module Fold_Map_Any : functor (M : MONOID_ANY) -> sig
    val fold_map : ('a -> 'a M.t) -> ('b -> 'a M.t) -> ('a, 'b) t -> 'a M.t
  end

  module Fold_Map_Plus : functor (P : PLUS) -> sig
    val fold_map : ('a -> 'a P.t) -> ('b -> 'a P.t) -> ('a, 'b) t -> 'a P.t
  end
end

module type BITRAVERSABLE = sig
  include BIFUNCTOR

  include BIFOLDABLE with type ('a, 'b) t := ('a, 'b) t

  type 'a applicative_t

  val bitraverse :
    ('a -> 'c applicative_t) -> ('b -> 'd applicative_t) -> ('a, 'b) t -> ('c, 'd) t applicative_t

  val bisequence : ('a applicative_t, 'b applicative_t) t -> ('a, 'b) t applicative_t
end

module type BITRAVERSABLE_F = functor (A : APPLICATIVE) ->
  BITRAVERSABLE with type 'a applicative_t = 'a A.t

module type BICONTRAVARIANT = sig
  type ('a, 'b) t

  val bicmap : ('b -> 'a) -> ('d -> 'c) -> ('a, 'c) t -> ('b, 'd) t
end
