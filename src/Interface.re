/* Note:
 * Any data structure implementing one of these interfaces must also satisfy the
 * corresponding laws in the `Verified` module
 */

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
