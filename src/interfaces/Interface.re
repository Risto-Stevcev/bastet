/* Note:
 * Any data structure implementing one of these interfaces must also satisfy the
 * corresponding laws in the `Verified` module
 */
module type TYPE = {
  type t;
};

module type MAGMA = {
  type t;
  let append: (t, t) => t;
};

module type MEDIAL_MAGMA = {
  include MAGMA;
};

module type MAGMA_ANY = {
  type t('a);
  let append: (t('a), t('a)) => t('a);
};

module type SEMIGROUP = {
  include MAGMA;
};

module type SEMIGROUP_ANY = {
  include MAGMA_ANY;
};

module type MONOID = {
  /* A Monoid only needs to be commutative with the empty element */
  include SEMIGROUP;
  let empty: t;
};

module type MONOID_ANY = {
  include SEMIGROUP_ANY;
  let empty: t('a);
};

module type QUASIGROUP = {
  include MAGMA;
};

module type MEDIAL_QUASIGROUP = {
  /* Every medial quasigroup is isotopic to an abelian group */
  include MEDIAL_MAGMA;
};

module type QUASIGROUP_ANY = {
  include MAGMA_ANY;
};

module type LOOP = {
  /* A Loop only needs to be commutative with the empty element */
  include QUASIGROUP;
  let empty: t;
};

module type LOOP_ANY = {
  /* A Loop only needs to be commutative with the empty element */
  include QUASIGROUP_ANY;
  let empty: t('a);
};

module type GROUP = {
  /* This is for documentation purposes only, include either LOOP or MONOID */
  include LOOP; /* or */ include MONOID with type t := t;
  let inverse: t => t;
};

module type GROUP_ANY = {
  /* This is for documentation purposes only, include either LOOP or MONOID */
  include LOOP_ANY; /* or */ include MONOID_ANY with type t('a) := t('a);
  let inverse: t('a) => t('a);
};

/* Every group is a loop */
module type GROUP_LOOP = (G: GROUP) => LOOP;
module type GROUP_LOOP_ANY = (G: GROUP_ANY) => LOOP_ANY;

module type ABELIAN_GROUP = {
  include GROUP;
};
module type ABELIAN_GROUP_ANY = {
  include GROUP_ANY;
};

module type FUNCTOR = {
  type t('a);
  let map: ('a => 'b, t('a)) => t('b);
};

module type APPLY = {
  include FUNCTOR;
  let apply: (t('a => 'b), t('a)) => t('b);
};

module type APPLICATIVE = {
  include APPLY;
  let pure: 'a => t('a);
};

module type MONAD = {
  include APPLICATIVE;
  let flat_map: (t('a), 'a => t('b)) => t('b);
};

module type ALT = {
  include FUNCTOR;
  let alt: (t('a), t('a)) => t('a);
};

module type PLUS = {
  include ALT;
  let empty: t('a);
};

module type ALTERNATIVE = {
  include APPLICATIVE;
  include PLUS with type t('a) := t('a);
};

module type FOLDABLE = {
  type t('a);
  let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
  let fold_right: (('b, 'a) => 'a, 'a, t('b)) => 'a;

  module Fold_Map: (M: MONOID) => {
    let fold_map: ('a => M.t, t('a)) => M.t;
  };
  module Fold_Map_Any: (M: MONOID_ANY) => {
    let fold_map: ('a => M.t('a), t('a)) => M.t('a);
  };
  module Fold_Map_Plus: (P: PLUS) => {
    let fold_map: ('a => P.t('a), t('a)) => P.t('a);
  };
};

module type TRAVERSABLE = {
  include FUNCTOR;
  include FOLDABLE with type t('a) := t('a);
  type applicative_t('a);

  let traverse: ('a => applicative_t('b), t('a)) => applicative_t(t('b));
  let sequence: t(applicative_t('a)) => applicative_t(t('a));
};

module type TRAVERSABLE_F = (A: APPLICATIVE) => TRAVERSABLE
  with type applicative_t('a) = A.t('a);

module type SEMIGROUPOID = {
  type t('a, 'b);
  let compose: (t('b, 'c), t('a, 'b)) => t('a, 'c);
};

module type CATEGORY = {
  include SEMIGROUPOID;
  let id: t('a, 'a);
};


module type EQ = {
  type t;
  let eq: (t, t) => bool;
};

module type QUASIREFLEXIVE_EQ = {
  include EQ;
};

module type EQ1 = {
  type t('a);
  let eq: (t('a), t('a)) => bool;
};

module type EQ2 = {
  type t('a, 'b);
  let eq: (t('a, 'b), t('a, 'b)) => bool;
};


type ordering = [ | `less_than | `equal_to | `greater_than ];

let invert = (ordering) => switch ordering {
  | `less_than => `greater_than
  | `greater_than => `less_than
  | `equal_to => `equal_to
  };

let unsafe_compare = (a, b) => a < b ? `less_than : a == b ? `equal_to : `greater_than;

module type ORD = {
  include EQ;
  let compare: (t, t) => ordering;
};

module Ordering = (O: ORD) => {
  let less_than: (O.t, O.t) => bool = (a, b) => O.compare(a, b) == `less_than;
  let greater_than: (O.t, O.t) => bool = (a, b) => O.compare(a, b) == `greater_than;
  let less_than_or_equal: (O.t, O.t) => bool =
    (a, b) => O.compare(a, b) != `greater_than;
  let greater_than_or_equal: (O.t, O.t) => bool =
    (a, b) => O.compare(a, b) != `less_than;
};


module type BOUNDED = {
  include ORD;
  let top: t;
  let bottom: t;
};

module type JOIN_SEMILATTICE = {
  type t;
  let join: (t, t) => t;
};

module type MEET_SEMILATTICE = {
  type t;
  let meet: (t, t) => t;
};

module type BOUNDED_JOIN_SEMILATTICE = {
  include JOIN_SEMILATTICE;
  let bottom: t;
};

module type BOUNDED_MEET_SEMILATTICE = {
  include MEET_SEMILATTICE;
  let top: t;
};

module type LATTICE = {
  include JOIN_SEMILATTICE;
  include MEET_SEMILATTICE with type t := t;
};

module type BOUNDED_LATTICE = {
  include BOUNDED_JOIN_SEMILATTICE;
  include BOUNDED_MEET_SEMILATTICE with type t := t;
};

module type DISTRIBUTIVE_LATTICE = {
  include LATTICE;
};

module type BOUNDED_DISTRIBUTIVE_LATTICE = {
  include BOUNDED_LATTICE;
};

module type HEYTING_ALGEBRA = {
  include ORD;
  include BOUNDED_DISTRIBUTIVE_LATTICE with type t := t;

  let not: t => t;
  let implies: (t, t) => t;
};

module type INVOLUTIVE_HEYTING_ALGEBRA = {
  include HEYTING_ALGEBRA;
};

module type BOOLEAN_ALGEBRA = {
  include HEYTING_ALGEBRA;
};

module type SHOW = {
  type t;
  let show: t => string;
};

module type SEMIRING = {
  type t;
  let add: (t, t) => t;
  let zero: t;
  let multiply: (t, t) => t;
  let one: t;
};

module type RING = {
  include SEMIRING;
  let subtract: (t, t) => t;
};

module type COMMUTATIVE_RING = {
  include RING;
};

module type DIVISION_RING = {
  include RING;
  let reciprocal: t => t;
};

module type EUCLIDEAN_RING = {
  include COMMUTATIVE_RING;
  let degree: t => int;
  let divide: (t, t) => t;
  let modulo: (t, t) => t;
};

module type FIELD = {
  include EUCLIDEAN_RING;
  include DIVISION_RING with type t := t;
};

module type INVARIANT = {
  type t('a);
  let imap: ('a => 'b, 'b => 'a, t('a)) => t('b);
};

module type CONTRAVARIANT = {
  type t('a);
  let cmap: ('b => 'a, t('a)) => t('b);
};

module type PROFUNCTOR = {
  type t('a, 'b);
  let dimap: ('a => 'b, 'c => 'd, t('b, 'c)) => t('a, 'd);
};

module type MONAD_ZERO = {
  include MONAD;
  include ALTERNATIVE with type t('a) := t('a);
};

module type MONAD_PLUS = {
  include MONAD_ZERO;
};

module type EXTEND = {
  include FUNCTOR;
  let extend: (t('a) => 'b, t('a)) => t('b);
};

module type COMONAD = {
  include EXTEND;
  let extract: t('a) => 'a;
};

module type BIFUNCTOR = {
  type t('a, 'b);
  let bimap: ('a => 'b, 'c => 'd, t('a, 'c)) => t('b, 'd);
};

module type BIAPPLY = {
  include BIFUNCTOR;
  let biapply: (t('a => 'b, 'c => 'd), t('a, 'c)) => t('b, 'd);
};

module type BIAPPLICATIVE = {
  include BIAPPLY;
  let bipure: ('a, 'b) => t('a, 'b);
};

module type BIFOLDABLE = {
  type t('a, 'b);
  let bifold_left: (('c, 'a) => 'c, ('c, 'b) => 'c, 'c, t('a, 'b)) => 'c;
  let bifold_right: (('a, 'c) => 'c, ('b, 'c) => 'c, 'c, t('a, 'b)) => 'c;

  module Fold_Map: (M: MONOID) => {
    let fold_map: ('a => M.t, 'b => M.t, t('a, 'b)) => M.t;
  };
  module Fold_Map_Any: (M: MONOID_ANY) => {
    let fold_map: ('a => M.t('a), 'b => M.t('a), t('a, 'b)) => M.t('a);
  };
  module Fold_Map_Plus: (P: PLUS) => {
    let fold_map: ('a => P.t('a), 'b => P.t('a), t('a, 'b)) => P.t('a);
  };
};

module type BITRAVERSABLE = {
  include BIFUNCTOR;
  include BIFOLDABLE with type t('a, 'b) := t('a, 'b);
  type applicative_t('a);

  let bitraverse: ('a => applicative_t('c), 'b => applicative_t('d), t('a, 'b)) => applicative_t(t('c, 'd));
  let bisequence: t(applicative_t('a), applicative_t('b)) => applicative_t(t('a, 'b));
};


module type BITRAVERSABLE_F = (A: APPLICATIVE) => BITRAVERSABLE
  with type applicative_t('a) = A.t('a);

