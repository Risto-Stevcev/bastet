val zip_with : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array

val zip : 'a array -> 'b array -> ('a * 'b) array

module type EQ_F = functor (E : Interface.EQ) -> sig
  type t = E.t array

  val eq : t -> t -> bool
end

module type ORD_F = functor (O : Interface.ORD) -> sig
  type t = O.t array

  val eq : t -> t -> bool

  val compare : t -> t -> Interface.ordering
end

module type SHOW_F = functor (S : Interface.SHOW) -> sig
  type t = S.t array

  val show : t -> string
end

module type TRAVERSABLE_F = functor (A : Interface.APPLICATIVE) -> sig
  type 'a t = 'a array

  val map : ('a -> 'b) -> 'a t -> 'b t

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  val fold_right : ('b -> 'a -> 'a) -> 'a -> 'b t -> 'a

  module Fold_Map : functor (M : Interface.MONOID) -> sig
    val fold_map : ('a -> M.t) -> 'a t -> M.t
  end

  module Fold_Map_Any : functor (M : Interface.MONOID_ANY) -> sig
    val fold_map : ('a -> 'b M.t) -> 'a t -> 'b M.t
  end

  module Fold_Map_Plus : functor (P : Interface.PLUS) -> sig
    val fold_map : ('a -> 'b P.t) -> 'a t -> 'b P.t
  end

  type 'a applicative_t = 'a A.t

  val traverse : ('a -> 'b applicative_t) -> 'a t -> 'b t applicative_t

  val sequence : 'a applicative_t t -> 'a t applicative_t
end

module Functor : sig
  type 'a t = 'a array

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Alt : sig
  type 'a t = 'a array

  val map : ('a -> 'b) -> 'a t -> 'b t

  val alt : 'a t -> 'a t -> 'a t
end

module Apply : sig
  type 'a t = 'a array

  val map : ('a -> 'b) -> 'a t -> 'b t

  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

module Applicative : sig
  type 'a t = 'a array

  val map : ('a -> 'b) -> 'a t -> 'b t

  val apply : ('a -> 'b) t -> 'a t -> 'b t

  val pure : 'a -> 'a t
end

module Monad : sig
  type 'a t = 'a array

  val map : ('a -> 'b) -> 'a t -> 'b t

  val apply : ('a -> 'b) t -> 'a t -> 'b t

  val pure : 'a -> 'a t

  val flat_map : 'a t -> ('a -> 'b t) -> 'b t
end

module Foldable : sig
  type 'a t = 'a array

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  val fold_right : ('b -> 'a -> 'a) -> 'a -> 'b t -> 'a

  module Fold_Map : functor (M : Interface.MONOID) -> sig
    val fold_map : ('a -> M.t) -> 'a t -> M.t
  end

  module Fold_Map_Any : functor (M : Interface.MONOID_ANY) -> sig
    val fold_map : ('a -> 'b M.t) -> 'a t -> 'b M.t
  end

  module Fold_Map_Plus : functor (P : Interface.PLUS) -> sig
    val fold_map : ('a -> 'b P.t) -> 'a t -> 'b P.t
  end
end

module Unfoldable : sig
  type 'a t = 'a array

  val unfold : ('a -> ('a * 'a) option) -> 'a -> 'a t
end

module Traversable : TRAVERSABLE_F

module Eq : EQ_F

module Ord : ORD_F

module Show : SHOW_F

module Invariant : sig
  type 'a t = 'a array

  val imap : ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t
end

module Extend : sig
  type 'a t = 'a array

  val map : ('a -> 'b) -> 'a t -> 'b t

  val extend : ('a t -> 'b) -> 'a t -> 'b t
end

module Infix : sig
  val ( <$> ) : ('a -> 'b) -> 'a Monad.t -> 'b Monad.t

  val ( <@> ) : 'a Monad.t -> ('a -> 'b) -> 'b Monad.t

  val ( <*> ) : ('a -> 'b) Monad.t -> 'a Monad.t -> 'b Monad.t

  val ( >>= ) : 'a Monad.t -> ('a -> 'b Monad.t) -> 'b Monad.t

  val ( =<< ) : ('a -> 'b Monad.t) -> 'a Monad.t -> 'b Monad.t

  val ( >=> ) : ('a -> 'b Monad.t) -> ('b -> 'c Monad.t) -> 'a -> 'c Monad.t

  val ( <=< ) : ('a -> 'b Monad.t) -> ('c -> 'a Monad.t) -> 'c -> 'b Monad.t

  val ( <<= ) : ('a Extend.t -> 'b) -> 'a Extend.t -> 'b Extend.t

  val ( =>> ) : 'a Extend.t -> ('a Extend.t -> 'b) -> 'b Extend.t
end
