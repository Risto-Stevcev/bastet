open Interface

let ( <. ) = Function.Infix.( <. )

[%%raw
{|
var fold_left = function(f, initial, a) {
  return Object.keys(a).reduce(function(acc, key) {
    return f(acc, a[key])
  }, initial)
};

var fold_left_keys = function(f, initial, a) {
  return Object.keys(a).reduce(function(acc, key) {
    return f(acc, key, a[key])
  }, initial)
};

var merge = function(a, b) {
  var obj = {}
  for (var key in b) obj[key] = b[key]
  for (var key in a) obj[key] = a[key]
  return obj
};
|}]

external fold_left : ('a -> 'b -> 'a) -> 'a -> 'b Js.Dict.t -> 'a = "fold_left" [@@bs.val]

external fold_left_keys : ('a -> string -> 'b -> 'a) -> 'a -> 'b Js.Dict.t -> 'a = "fold_left_keys"
  [@@bs.val]

external merge : 'a Js.Dict.t -> 'a Js.Dict.t -> 'a Js.Dict.t = "merge" [@@bs.val]

external unsafe_from_object : 'a Js.t -> 'b Js.Dict.t = "%identity"

let insert =
  (fun key value dict ->
     Js.Dict.set dict key value;
     dict
    : string -> 'a -> 'a Js.Dict.t -> 'a Js.Dict.t)

module type TRAVERSABLE_F = functor (A : APPLICATIVE) ->
  TRAVERSABLE with type 'a t = 'a Js.Dict.t and type 'a applicative_t = 'a A.t

module Functor : FUNCTOR with type 'a t = 'a Js.Dict.t = struct
  type 'a t = 'a Js.Dict.t

  let map f a = Js.Dict.map (fun [@bs] x -> f x) a
end

module Apply : APPLY with type 'a t = 'a Js.Dict.t = struct
  include Functor

  let apply fn_array a =
    fold_left (fun acc f -> merge acc (map f a)) (Obj.magic (Js.Dict.empty ())) fn_array
end

module Alt : ALT with type 'a t = 'a Js.Dict.t = struct
  include Functor

  let alt = merge
end

module Plus : PLUS with type 'a t = 'a Js.Dict.t = struct
  include Alt

  let empty = Obj.magic (Js.Dict.empty ())
end

module Foldable : FOLDABLE with type 'a t = 'a Js.Dict.t = struct
  type 'a t = 'a Js.Dict.t

  let fold_left = fold_left

  and fold_right f init a = ArrayLabels.fold_right ~f ~init (Js.Dict.values a)

  module Fold_Map (M : MONOID) = struct
    module D =
      Default.Fold_Map
        (M)
        (struct
          type 'a t = 'a Js.Dict.t

          let fold_left, fold_right = fold_left, fold_right
        end)

    let fold_map = D.fold_map_default_left
  end

  module Fold_Map_Any (M : MONOID_ANY) = struct
    module D =
      Default.Fold_Map_Any
        (M)
        (struct
          type 'a t = 'a Js.Dict.t

          let fold_left, fold_right = fold_left, fold_right
        end)

    let fold_map = D.fold_map_default_left
  end

  module Fold_Map_Plus (P : PLUS) = struct
    module D =
      Default.Fold_Map_Plus
        (P)
        (struct
          type 'a t = 'a Js.Dict.t

          let fold_left, fold_right = fold_left, fold_right
        end)

    let fold_map = D.fold_map_default_left
  end
end

module Traversable : TRAVERSABLE_F =
functor
  (A : APPLICATIVE)
  ->
  struct
    type 'a t = 'a Js.Dict.t

    and 'a applicative_t = 'a A.t

    include (Functor : FUNCTOR with type 'a t := 'a t)

    include (Foldable : FOLDABLE with type 'a t := 'a t)

    module I = Infix.Apply (A)

    let traverse_with_index f a =
      let open I in
      fold_left_keys
        (fun acc k v -> Function.flip (insert k) <$> acc <*> f k v)
        (A.pure (Js.Dict.empty ()))
        a

    let traverse = Obj.magic (traverse_with_index <. Function.const)

    module D = Default.Sequence (struct
      type 'a t = 'a Js.Dict.t

      and 'a applicative_t = 'a A.t

      let traverse = traverse
    end)

    let sequence = D.sequence_default
  end
