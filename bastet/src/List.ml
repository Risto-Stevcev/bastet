open Interface

module type EQ_F = functor (E : EQ) -> EQ with type t = E.t list

module type SHOW_F = functor (S : SHOW) -> SHOW with type t = S.t list

module type TRAVERSABLE_F = functor (A : APPLICATIVE) ->
  TRAVERSABLE with type 'a t = 'a list and type 'a applicative_t = 'a A.t

module Functor : FUNCTOR with type 'a t = 'a list = struct
  type 'a t = 'a list

  let map f = ListLabels.map ~f
end

module Alt : ALT with type 'a t = 'a list = struct
  include Functor

  let alt = ListLabels.append
end

module Apply : APPLY with type 'a t = 'a list = struct
  include Functor

  let apply fn_array a =
    ListLabels.fold_left ~f:(fun acc f -> Alt.alt acc (map f a)) ~init:[] fn_array
end

module Applicative : APPLICATIVE with type 'a t = 'a list = struct
  include Apply

  let pure a = [a]
end

module Monad : MONAD with type 'a t = 'a list = struct
  include Applicative

  let flat_map x f = ListLabels.fold_left ~f:(fun acc a -> Alt.alt acc (f a)) ~init:[] x
end

module Plus : PLUS with type 'a t = 'a list = struct
  include Alt

  let empty = []
end

module Alternative : ALTERNATIVE with type 'a t = 'a list = struct
  include Applicative

  include (Plus : PLUS with type 'a t := 'a t)
end

module Foldable : FOLDABLE with type 'a t = 'a list = struct
  type 'a t = 'a list

  let fold_left f init = ListLabels.fold_left ~f ~init

  and fold_right f init = ListLabels.fold_right ~f ~init

  module Fold_Map (M : MONOID) = struct
    module D =
      Default.Fold_Map
        (M)
        (struct
          type 'a t = 'a list

          let fold_left, fold_right = fold_left, fold_right
        end)

    let fold_map = D.fold_map_default_left
  end

  module Fold_Map_Any (M : MONOID_ANY) = struct
    module D =
      Default.Fold_Map_Any
        (M)
        (struct
          type 'a t = 'a list

          let fold_left, fold_right = fold_left, fold_right
        end)

    let fold_map = D.fold_map_default_left
  end

  module Fold_Map_Plus (P : PLUS) = struct
    module D =
      Default.Fold_Map_Plus
        (P)
        (struct
          type 'a t = 'a list

          let fold_left, fold_right = fold_left, fold_right
        end)

    let fold_map = D.fold_map_default_left
  end
end

module Unfoldable : UNFOLDABLE with type 'a t = 'a list = struct
  type 'a t = 'a list

  let rec unfold f init =
    match f init with
    | Some (a, next) -> a :: unfold f next
    | None -> []
end

module Traversable : TRAVERSABLE_F =
functor
  (A : APPLICATIVE)
  ->
  struct
    type 'a t = 'a list

    and 'a applicative_t = 'a A.t

    include (Functor : FUNCTOR with type 'a t := 'a t)

    include (Foldable : FOLDABLE with type 'a t := 'a t)

    module I = Infix.Apply (A)

    let traverse f =
      let open I in
      ListLabels.fold_right
        ~f:(fun acc x -> A.pure (fun y ys -> y :: ys) <*> f acc <*> x)
        ~init:(A.pure [])

    module D = Default.Sequence (struct
      type 'a t = 'a list

      type 'a applicative_t = 'a A.t

      let traverse = traverse
    end)

    let sequence = D.sequence_default
  end

module Eq : EQ_F =
functor
  (E : EQ)
  ->
  struct
    type t = E.t list

    let eq xs ys =
      ListLabels.length xs = ListLabels.length ys
      && ListLabels.fold_left
           ~f:(fun acc (a, b) -> acc && E.eq a b)
           ~init:true
           (ListLabels.combine xs ys)
  end

module Show : SHOW_F =
functor
  (S : SHOW)
  ->
  struct
    module F = Functions.Foldable (Foldable)
    module M = F.Monoid (String.Monoid)

    type t = S.t list

    let show xs = "[" ^ M.intercalate ~separator:", " (Functor.map S.show xs) ^ "]"
  end

module Infix = struct
  include Infix.Monad (Monad)
  include Infix.Alternative (Alternative)
end
