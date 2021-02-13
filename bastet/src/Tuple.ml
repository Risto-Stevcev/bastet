open Interface

let first (a, _) = a

and second (_, b) = b

and swap a b = b, a

and curry f a b = f (a, b)

and uncurry f (a, b) = f a b

module type MAGMA_F = functor (First : MAGMA) (Second : MAGMA) ->
  MAGMA with type t = First.t * Second.t

module type SEMIGROUP_F = functor (First : SEMIGROUP) (Second : SEMIGROUP) ->
  SEMIGROUP with type t = First.t * Second.t

module type MONOID_F = functor (First : MONOID) (Second : MONOID) ->
  MONOID with type t = First.t * Second.t

module type FUNCTOR_F = functor (T : TYPE) -> FUNCTOR with type 'a t = T.t * 'a

module type APPLY_F = functor (S : SEMIGROUP) -> APPLY with type 'a t = S.t * 'a

module type APPLICATIVE_F = functor (M : MONOID) -> APPLICATIVE with type 'a t = M.t * 'a

module type MONAD_F = functor (M : MONOID) -> MONAD with type 'a t = M.t * 'a

module type FOLDABLE_F = functor (T : TYPE) -> FOLDABLE with type 'a t = T.t * 'a

module type EQ_F = functor (First : EQ) (Second : EQ) -> EQ with type t = First.t * Second.t

module type SHOW_F = functor (First : SHOW) (Second : SHOW) -> SHOW with type t = First.t * Second.t

module type TRAVERSABLE_F = functor (T : TYPE) (A : APPLICATIVE) ->
  TRAVERSABLE with type 'a t = T.t * 'a and type 'a applicative_t = 'a A.t

module Magma : MAGMA_F =
functor
  (First : MAGMA)
  (Second : MAGMA)
  ->
  struct
    type t = First.t * Second.t

    let append (a, b) (a', b') = First.append a a', Second.append b b'
  end

module Semigroup : SEMIGROUP_F =
functor
  (First : SEMIGROUP)
  (Second : SEMIGROUP)
  ->
  struct
    include Magma (First) (Second)
  end

module Monoid : MONOID_F =
functor
  (First : MONOID)
  (Second : MONOID)
  ->
  struct
    include Semigroup (First) (Second)

    let empty = First.empty, Second.empty
  end

module Functor : FUNCTOR_F =
functor
  (T : TYPE)
  ->
  struct
    type 'a t = T.t * 'a

    let map f (a, b) = a, f b
  end

module Apply : APPLY_F =
functor
  (S : SEMIGROUP)
  ->
  struct
    include Functor (S)

    let apply (a, f) (a', x) = S.append a a', f x
  end

module Applicative : APPLICATIVE_F =
functor
  (M : MONOID)
  ->
  struct
    include Apply (M)

    let pure a = M.empty, a
  end

module Monad : MONAD_F =
functor
  (M : MONOID)
  ->
  struct
    include Applicative (M)

    let flat_map (a, b) f =
      match f b with
      | a', c -> M.append a a', c
  end

module Foldable : FOLDABLE_F =
functor
  (T : TYPE)
  ->
  struct
    type 'a t = T.t * 'a

    let fold_left f init (_, x) = f init x

    and fold_right f init (_, x) = f x init

    module Fold = struct
      let fold_map f (_, x) = f x
    end

    module Fold_Map (M : MONOID) = struct
      include Fold
    end

    module Fold_Map_Plus (P : PLUS) = struct
      include Fold
    end

    module Fold_Map_Any (M : MONOID_ANY) = struct
      include Fold
    end
  end

module Traversable : TRAVERSABLE_F =
functor
  (T : TYPE)
  (A : APPLICATIVE)
  ->
  struct
    type 'a t = T.t * 'a

    and 'a applicative_t = 'a A.t

    include (Functor (T) : FUNCTOR with type 'a t := 'a t)

    include (Foldable (T) : FOLDABLE with type 'a t := 'a t)

    let traverse f (x, y) = A.map (fun z -> x, z) (f y)

    and sequence (x, y) = A.map (fun z -> x, z) y
  end

module Eq : EQ_F =
functor
  (First : EQ)
  (Second : EQ)
  ->
  struct
    type t = First.t * Second.t

    let eq (a, b) (a', b') = First.eq a a' && Second.eq b b'
  end

module Semigroupoid : SEMIGROUPOID with type ('a, 'b) t = 'a * 'b = struct
  type ('a, 'b) t = 'a * 'b

  let compose (_, c) (a, _) = a, c
end

module Show : SHOW_F =
functor
  (First : SHOW)
  (Second : SHOW)
  ->
  struct
    type t = First.t * Second.t

    let show (a, b) = "(" ^ First.show a ^ ", " ^ Second.show b ^ ")"
  end

module Bifunctor : BIFUNCTOR with type ('a, 'b) t = 'a * 'b = struct
  type ('a, 'b) t = 'a * 'b

  let bimap f g (a, b) = f a, g b
end

module Biapply : BIAPPLY with type ('a, 'b) t = 'a * 'b = struct
  include Bifunctor

  let biapply (f, g) (a, b) = f a, g b
end

module Biapplicative : BIAPPLICATIVE with type ('a, 'b) t = 'a * 'b = struct
  include Biapply

  let bipure a b = a, b
end

module Bifoldable : BIFOLDABLE with type ('a, 'b) t = 'a * 'b = struct
  type ('a, 'b) t = 'a * 'b

  let bifold_left f g init (a, b) = g (f init a) b

  and bifold_right f g init (a, b) = f a (g b init)

  module Fold_Map (M : MONOID) = struct
    let fold_map f g (a, b) = M.append (f a) (g b)
  end

  module Fold_Map_Any (M : MONOID_ANY) = struct
    let fold_map f g (a, b) = M.append (f a) (g b)
  end

  module Fold_Map_Plus (P : PLUS) = struct
    let fold_map f g (a, b) = P.alt (f a) (g b)
  end
end

module type BITRAVERSABLE_F = functor (A : APPLICATIVE) ->
  BITRAVERSABLE with type 'a applicative_t = 'a A.t and type ('a, 'b) t = 'a * 'b

module Bitraversable : BITRAVERSABLE_F =
functor
  (A : APPLICATIVE)
  ->
  struct
    type ('a, 'b) t = 'a * 'b

    and 'a applicative_t = 'a A.t

    include (Bifunctor : BIFUNCTOR with type ('a, 'b) t := ('a, 'b) t)

    include (Bifoldable : BIFOLDABLE with type ('a, 'b) t := ('a, 'b) t)

    module I = Infix.Apply (A)

    let bitraverse f g (a, b) =
      let open I in
      A.map (fun a b -> a, b) (f a) <*> g b

    and bisequence (a, b) =
      let open I in
      A.map (fun a b -> a, b) a <*> b
  end

module Infix = struct
  include Infix.Biapply (Biapply)
end
