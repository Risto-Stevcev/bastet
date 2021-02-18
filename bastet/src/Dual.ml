open Interface

type 'a dual = Dual of 'a

module type MAGMA_F = functor (M : MAGMA) -> MAGMA with type t = M.t dual

module type SEMIGROUP_F = functor (S : SEMIGROUP) -> SEMIGROUP with type t = S.t dual

module type MONOID_F = functor (M : MONOID) -> MONOID with type t = M.t dual

module type MAGMA_ANY_F = functor (M : MAGMA_ANY) -> MAGMA_ANY with type 'a t = 'a M.t dual

module type SEMIGROUP_ANY_F = functor (S : SEMIGROUP_ANY) ->
  SEMIGROUP_ANY with type 'a t = 'a S.t dual

module type MONOID_ANY_F = functor (M : MONOID_ANY) -> MONOID_ANY with type 'a t = 'a M.t dual

module type TRAVERSABLE_F = functor (A : APPLICATIVE) ->
  TRAVERSABLE with type 'a t = 'a dual and type 'a applicative_t = 'a A.t

module Magma : MAGMA_F =
functor
  (M : MAGMA)
  ->
  struct
    type t = M.t dual

    let append (Dual a) (Dual b) = Dual (M.append b a)
  end

module Semigroup : SEMIGROUP_F =
functor
  (S : SEMIGROUP)
  ->
  struct
    include Magma (S)
  end

module Monoid : MONOID_F =
functor
  (M : MONOID)
  ->
  struct
    include Semigroup (M)

    let empty = Dual M.empty
  end

module Functor : FUNCTOR with type 'a t = 'a dual = struct
  type 'a t = 'a dual

  let map f (Dual a) = Dual (f a)
end

module Applicative : APPLICATIVE with type 'a t = 'a dual = struct
  include Functor

  let apply (Dual f) (Dual a) = Dual (f a)

  let pure a = Dual a
end

module Monad : MONAD with type 'a t = 'a dual = struct
  include Applicative

  let flat_map (Dual a) f = f a
end

module Magma_Any : MAGMA_ANY_F =
functor
  (M : MAGMA_ANY)
  ->
  struct
    type 'a t = 'a M.t dual

    let append (Dual a) (Dual b) = Dual (M.append b a)
  end

module Semigroup_Any : SEMIGROUP_ANY_F =
functor
  (S : SEMIGROUP_ANY)
  ->
  struct
    include Magma_Any (S)
  end

module Monoid_Any : MONOID_ANY_F =
functor
  (M : MONOID_ANY)
  ->
  struct
    include Semigroup_Any (M)

    let empty = Dual M.empty
  end

module Foldable : FOLDABLE with type 'a t = 'a dual = struct
  type 'a t = 'a dual

  let fold_left f init (Dual x) = f init x

  and fold_right f init (Dual x) = f x init

  module Fold = struct
    let fold_map f (Dual x) = f x
  end

  module Fold_Map (M : MONOID) = struct
    include Fold
  end

  module Fold_Map_Any (M : MONOID_ANY) = struct
    include Fold
  end

  module Fold_Map_Plus (P : PLUS) = struct
    include Fold
  end
end

module Traversable : TRAVERSABLE_F =
functor
  (A : APPLICATIVE)
  ->
  struct
    type 'a t = 'a dual

    and 'a applicative_t = 'a A.t

    include (Functor : FUNCTOR with type 'a t := 'a t)

    include (Foldable : FOLDABLE with type 'a t := 'a t)

    module I = Infix.Functor (A)

    let traverse f x =
      let open I in
      match x with
      | Dual x' -> (fun x -> Dual x) <$> f x'

    let sequence x =
      let open I in
      match x with
      | Dual x' -> (fun x -> Dual x) <$> x'
  end

module Infix = struct
  include Infix.Monad (Monad)
end
