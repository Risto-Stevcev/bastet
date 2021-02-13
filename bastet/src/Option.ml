open Interface

let ( <. ) = Function.Infix.( <. )

let maybe =
  (fun ~f ~default opt ->
     match opt with
     | ((Some a)[@explicit_arity]) -> f a
     | None -> default
    : f:('a -> 'b) -> default:'b -> 'a option -> 'b)

let getWithDefault a x =
  match x with
  | None -> a
  | ((Some x)[@explicit_arity]) -> x

module type MAGMA_F = functor (M : MAGMA) -> MAGMA with type t = M.t option

module type SEMIGROUP_F = functor (S : SEMIGROUP) -> SEMIGROUP with type t = S.t option

module type MONOID_F = functor (S : SEMIGROUP) -> MONOID with type t = S.t option

module type QUASIGROUP_F = functor (Q : QUASIGROUP) -> QUASIGROUP with type t = Q.t option

module type LOOP_F = functor (L : LOOP) -> LOOP with type t = L.t option

module type EQ_F = functor (E : EQ) -> EQ with type t = E.t option

module type ORD_F = functor (O : ORD) -> ORD with type t = O.t option

module type SHOW_F = functor (S : SHOW) -> SHOW with type t = S.t option

module type TRAVERSABLE_F = functor (A : APPLICATIVE) ->
  TRAVERSABLE with type 'a t = 'a option and type 'a applicative_t = 'a A.t

module Functor : FUNCTOR with type 'a t = 'a option = struct
  type 'a t = 'a option

  let map f a =
    match a with
    | ((Some a')[@explicit_arity]) -> Some (f a') [@explicit_arity]
    | None -> None
end

module Apply : APPLY with type 'a t = 'a option = struct
  include Functor

  let apply fn_opt a =
    match fn_opt with
    | ((Some f)[@explicit_arity]) -> map f a
    | None -> None
end

module Applicative : APPLICATIVE with type 'a t = 'a option = struct
  include Apply

  let pure a = (Some a [@explicit_arity])
end

module Monad : MONAD with type 'a t = 'a option = struct
  include Applicative

  let flat_map x f =
    match x with
    | ((Some x')[@explicit_arity]) -> f x'
    | None -> None
end

module Magma : MAGMA_F =
functor
  (M : MAGMA)
  ->
  struct
    type t = M.t option

    let append a b =
      match a, b with
      | ((Some a)[@explicit_arity]), ((Some b)[@explicit_arity]) ->
          Some (M.append a b) [@explicit_arity]
      | ((Some a)[@explicit_arity]), _ | _, ((Some a)[@explicit_arity]) -> Some a [@explicit_arity]
      | _ -> None
  end

module Semigroup (S : SEMIGROUP) = struct
  include Magma (S)
end

module Monoid : MONOID_F =
functor
  (S : SEMIGROUP)
  ->
  struct
    include Semigroup (S)

    let empty = None
  end

module Quasigroup : QUASIGROUP_F =
functor
  (Q : QUASIGROUP)
  ->
  struct
    include Magma (Q)
  end

module Loop : LOOP_F =
functor
  (L : LOOP)
  ->
  struct
    include Quasigroup (L)

    let empty = None
  end

module Alt : ALT with type 'a t = 'a option = struct
  include Functor

  let alt a b =
    match a, b with
    | ((Some a)[@explicit_arity]), _ -> Some a [@explicit_arity]
    | None, a -> a
end

module Plus : PLUS with type 'a t = 'a option = struct
  include Alt

  let empty = None
end

module Alternative : ALTERNATIVE with type 'a t = 'a option = struct
  include Applicative

  include (Plus : PLUS with type 'a t := 'a t)
end

module Foldable : FOLDABLE with type 'a t = 'a option = struct
  type 'a t = 'a option

  let fold_left f init x = maybe ~f:(f init) ~default:init x

  and fold_right f init x = maybe ~f:(fun x' -> f x' init) ~default:init x

  module Fold_Map (M : MONOID) = struct
    let fold_map f x = maybe ~f ~default:M.empty x
  end

  module Fold_Map_Any (M : MONOID_ANY) = struct
    let fold_map f x = maybe ~f ~default:M.empty x
  end

  module Fold_Map_Plus (P : PLUS) = struct
    let fold_map f x = maybe ~f ~default:P.empty x
  end
end

module Traversable (A : APPLICATIVE) = struct
  type 'a t = 'a option

  and 'a applicative_t = 'a A.t

  include (Functor : FUNCTOR with type 'a t := 'a t)

  include (Foldable : FOLDABLE with type 'a t := 'a t)

  let traverse f x =
    maybe ~f:(A.map (fun a -> (Some a [@explicit_arity])) <. f) ~default:(A.pure None) x

  and sequence x = maybe ~f:(A.map (fun a -> (Some a [@explicit_arity]))) ~default:(A.pure None) x
end

module Eq : EQ_F =
functor
  (E : EQ)
  ->
  struct
    type t = E.t option

    let eq xs ys =
      match xs, ys with
      | ((Some a)[@explicit_arity]), ((Some b)[@explicit_arity]) -> E.eq a b
      | None, None -> true
      | _ -> false
  end

module Ord : ORD_F =
functor
  (O : ORD)
  ->
  struct
    include Eq (O)

    let compare a b =
      match a, b with
      | ((Some a')[@explicit_arity]), ((Some b')[@explicit_arity]) -> O.compare a' b'
      | None, None -> `equal_to
      | None, Some _ -> `less_than
      | Some _, None -> `greater_than
  end

module Show : SHOW_F =
functor
  (S : SHOW)
  ->
  struct
    type t = S.t option

    let show a =
      match a with
      | ((Some a')[@explicit_arity]) -> "Some(" ^ S.show a' ^ ")"
      | None -> "None"
  end

module Infix = struct
  include Infix.Monad (Monad)
  include Infix.Alternative (Alternative)

  let ( |? ) = getWithDefault
end
