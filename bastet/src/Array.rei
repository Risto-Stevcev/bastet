let zip_with: (('a, 'b) => 'c, array('a), array('b)) => array('c);
let zip: (array('a), array('b)) => array(('a, 'b));
module type EQ_F =
  (E: Interface.EQ) => {
                         type t = array(E.t);
                         let eq: (t, t) => bool;
                       };
module type ORD_F =
  (O: Interface.ORD) =>
   {
    type t = array(O.t);
    let eq: (t, t) => bool;
    let compare: (t, t) => Interface.ordering;
  };
module type SHOW_F =
  (S: Interface.SHOW) => {
                           type t = array(S.t);
                           let show: t => string;
                         };
module type TRAVERSABLE_F =
  (A: Interface.APPLICATIVE) =>
   {
    type t('a) = array('a);
    let map: ('a => 'b, t('a)) => t('b);
    let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
    let fold_right: (('b, 'a) => 'a, 'a, t('b)) => 'a;
    module Fold_Map:
      (M: Interface.MONOID) => {let fold_map: ('a => M.t, t('a)) => M.t;};
    module Fold_Map_Any:
      (M: Interface.MONOID_ANY) =>
       {let fold_map: ('a => M.t('b), t('a)) => M.t('b);};
    module Fold_Map_Plus:
      (P: Interface.PLUS) =>
       {let fold_map: ('a => P.t('b), t('a)) => P.t('b);};
    type applicative_t('a) = A.t('a);
    let traverse: ('a => applicative_t('b), t('a)) => applicative_t(t('b));
    let sequence: t(applicative_t('a)) => applicative_t(t('a));
  };
module Functor: {
  type t('a) = array('a);
  let map: ('a => 'b, t('a)) => t('b);
};
module Alt: {
  type t('a) = array('a);
  let map: ('a => 'b, t('a)) => t('b);
  let alt: (t('a), t('a)) => t('a);
};
module Apply: {
  type t('a) = array('a);
  let map: ('a => 'b, t('a)) => t('b);
  let apply: (t('a => 'b), t('a)) => t('b);
};
module Applicative: {
  type t('a) = array('a);
  let map: ('a => 'b, t('a)) => t('b);
  let apply: (t('a => 'b), t('a)) => t('b);
  let pure: 'a => t('a);
};
module Monad: {
  type t('a) = array('a);
  let map: ('a => 'b, t('a)) => t('b);
  let apply: (t('a => 'b), t('a)) => t('b);
  let pure: 'a => t('a);
  let flat_map: (t('a), 'a => t('b)) => t('b);
};
module Foldable: {
  type t('a) = array('a);
  let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
  let fold_right: (('b, 'a) => 'a, 'a, t('b)) => 'a;
  module Fold_Map:
    (M: Interface.MONOID) => {let fold_map: ('a => M.t, t('a)) => M.t;};
  module Fold_Map_Any:
    (M: Interface.MONOID_ANY) =>
     {let fold_map: ('a => M.t('b), t('a)) => M.t('b);};
  module Fold_Map_Plus:
    (P: Interface.PLUS) =>
     {let fold_map: ('a => P.t('b), t('a)) => P.t('b);};
};
module Unfoldable: {
  type t('a) = array('a);
  let unfold: ('a => option(('a, 'a)), 'a) => t('a);
};
module Traversable: TRAVERSABLE_F;
module Eq: EQ_F;
module Ord: ORD_F;
module Show: SHOW_F;
module Invariant: {
  type t('a) = array('a);
  let imap: ('a => 'b, 'b => 'a, t('a)) => t('b);
};
module Extend: {
  type t('a) = array('a);
  let map: ('a => 'b, t('a)) => t('b);
  let extend: (t('a) => 'b, t('a)) => t('b);
};
module Infix: {
  let (<$>): ('a => 'b, Monad.t('a)) => Monad.t('b);
  let (<#>): (Monad.t('a), 'a => 'b) => Monad.t('b);
  let (<*>): (Monad.t('a => 'b), Monad.t('a)) => Monad.t('b);
  let (>>=): (Monad.t('a), 'a => Monad.t('b)) => Monad.t('b);
  let (=<<): ('a => Monad.t('b), Monad.t('a)) => Monad.t('b);
  let (>=>): ('a => Monad.t('b), 'b => Monad.t('c), 'a) => Monad.t('c);
  let (<=<): ('a => Monad.t('b), 'c => Monad.t('a), 'c) => Monad.t('b);
  let (<<=): (Extend.t('a) => 'b, Extend.t('a)) => Extend.t('b);
  let (=>>): (Extend.t('a), Extend.t('a) => 'b) => Extend.t('b);
};
