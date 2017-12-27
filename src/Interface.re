/* Note:
 * Any data structure implementing one of these interfaces must also satisfy the
 * corresponding laws in the `Verified` module
 */
module type TYPE = {
  type t;
};

module type SEMIGROUP = {
  type t;
  let append: (t, t) => t;
};

module type SEMIGROUP_ANY = {
  type t('a);
  let append: (t('a), t('a)) => t('a);
};

module type MONOID = {
  include SEMIGROUP;
  let empty: t;
};

module type MONOID_ANY = {
  include SEMIGROUP_ANY;
  let empty: t('a);
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
