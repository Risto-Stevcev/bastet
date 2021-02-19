(** This module provides default implementations for interfaces *)

let ( <. ) = Function.Infix.( <. )

module type FOLD = sig
  type 'a t

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  val fold_right : ('b -> 'a -> 'a) -> 'a -> 'b t -> 'a
end

module Fold_Map (M : Interface.MONOID) (F : FOLD) = struct
  module I = Infix.Magma (M)

  let fold_map_default_left f x =
    let open I in
    F.fold_left (fun acc x -> acc <:> f x) M.empty x

  and fold_map_default_right f x =
    let open I in
    F.fold_right (fun x acc -> f x <:> acc) M.empty x
end

module Fold_Map_Any (M : Interface.MONOID_ANY) (F : FOLD) = struct
  module I = Infix.Magma_Any (M)

  let fold_map_default_left f x =
    let open I in
    F.fold_left (fun acc x -> acc <:> f x) M.empty x

  and fold_map_default_right f x =
    let open I in
    F.fold_right (fun x acc -> f x <:> acc) M.empty x
end

module Fold_Map_Plus (P : Interface.PLUS) (F : FOLD) = struct
  module I = Infix.Alt (P)

  let fold_map_default_left f x =
    let open I in
    F.fold_left (fun acc x -> acc <|> f x) P.empty x

  and fold_map_default_right f x =
    let open I in
    F.fold_right (fun x acc -> f x <|> acc) P.empty x
end

module type FOLD_MAP = sig
  type 'a t

  module Fold_Map_Any : functor (M : Interface.MONOID_ANY) -> sig
    val fold_map : ('a -> 'b M.t) -> 'a t -> 'b M.t
  end

  module Fold_Map_Plus : functor (P : Interface.PLUS) -> sig
    val fold_map : ('a -> 'b P.t) -> 'a t -> 'b P.t
  end
end

module Fold (F : FOLD_MAP) = struct
  type 'a t = 'a F.t

  module Dual_Endo = Dual.Monoid_Any (Endo.Monoid)
  module Dual_Fold_Map = F.Fold_Map_Any (Dual_Endo)
  module Endo_Fold_Map = F.Fold_Map_Any (Endo.Monoid)

  let fold_left_default f init xs =
    let (Dual.Dual (Endo.Endo r)) =
      Dual_Fold_Map.fold_map ((fun x -> Dual.Dual (Endo.Endo x)) <. Function.flip f) xs
    in
    r init

  and fold_right_default f init xs =
    let (Endo.Endo r) = Endo_Fold_Map.fold_map ((fun x -> Endo.Endo x) <. f) xs in
    r init
end

module type TRAVERSE = sig
  type 'a t

  type 'a applicative_t

  val traverse : ('a -> 'b applicative_t) -> 'a t -> 'b t applicative_t
end

module type SEQUENCE = sig
  type 'a t

  type 'a applicative_t

  include Interface.FUNCTOR with type 'a t := 'a t

  val sequence : 'a applicative_t t -> 'a t applicative_t
end

module Sequence (T : TRAVERSE) = struct
  let sequence_default xs = T.traverse Function.Category.id xs
end

module Traverse (S : SEQUENCE) = struct
  let traverse_default f xs = S.sequence (S.map f xs)
end
