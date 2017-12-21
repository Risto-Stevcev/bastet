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

module type SEMIGROUPOID = {
  type t('a, 'b);
  let compose: (t('b, 'c), t('a, 'b)) => t('a, 'c);
};

module type CATEGORY = {
  include SEMIGROUPOID;
  let id: t('a, 'a);
};
