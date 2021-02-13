open Interface

let flip = (fun f b a -> f a b : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c)

and const = (fun a _ -> a : 'a -> 'b -> 'a)

module type FUNCTOR_F = functor (T : TYPE) -> FUNCTOR with type 'a t = T.t -> 'a

module type APPLY_F = functor (T : TYPE) -> APPLY with type 'a t = T.t -> 'a

module type INVARIANT_F = functor (T : TYPE) -> INVARIANT with type 'a t = T.t -> 'a

module type CONTRAVARIANT_F = functor (T : TYPE) -> CONTRAVARIANT with type 'a t = 'a -> T.t

module type BICONTRAVARIANT_F = functor (T : TYPE) ->
  BICONTRAVARIANT with type ('a, 'b) t = 'a -> 'b -> T.t

module Functor : FUNCTOR_F =
functor
  (T : TYPE)
  ->
  struct
    type 'b t = T.t -> 'b

    let map f g x = f (g x)
  end

module Apply : APPLY_F =
functor
  (T : TYPE)
  ->
  struct
    module Functor = Functor (T)
    include Functor

    let apply f g x = f x (g x)
  end

module Semigroupoid : SEMIGROUPOID with type ('a, 'b) t = 'a -> 'b = struct
  type ('a, 'b) t = 'a -> 'b

  let compose f g x = f (g x)
end

module Category : CATEGORY with type ('a, 'b) t = 'a -> 'b = struct
  include Semigroupoid

  let id a = a
end

module Invariant : INVARIANT_F =
functor
  (T : TYPE)
  ->
  struct
    module F = Functor (T)

    type 'b t = T.t -> 'b

    let imap f _ = F.map f
  end

module Profunctor : PROFUNCTOR with type ('a, 'b) t = 'a -> 'b = struct
  module I = Infix.Semigroupoid (Semigroupoid)

  let ( >. ) = I.( >. )

  type ('a, 'b) t = 'a -> 'b

  let dimap a_to_b c_to_d b_to_c = a_to_b >. b_to_c >. c_to_d
end

module Contravariant : CONTRAVARIANT_F =
functor
  (T : TYPE)
  ->
  struct
    type 'a t = 'a -> T.t

    let cmap f g x = g (f x)
  end

module Bicontravariant : BICONTRAVARIANT_F =
functor
  (T : TYPE)
  ->
  struct
    type ('a, 'b) t = 'a -> 'b -> T.t

    let bicmap f g h a b = h (f a) (g b)
  end

module Infix = struct
  include Infix.Semigroupoid (Semigroupoid)
end
