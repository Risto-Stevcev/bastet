open Interface

module type IMPL = sig
  val length : 'a array -> int

  val make : int -> 'a -> 'a array

  val append : 'a array -> 'a array -> 'a array

  val map : ('a -> 'b) -> 'a array -> 'b array

  val mapi : ('a -> int -> 'b) -> 'a array -> 'b array

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a

  val every : ('a -> bool) -> 'a array -> bool

  val slice : start:int -> end_:int -> 'a array -> 'a array
end

module type ARRAY = sig
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
end

module Make (A : IMPL) : ARRAY = struct
  let zip_with =
    (fun f xs ys ->
       let l =
         match A.length xs < A.length ys with
         | true -> A.length xs
         | false -> A.length ys
       and index = ref 0
       and result = ref None in
       for i = 0 to l - 1 do
         let value = f (ArrayLabels.get xs i) (ArrayLabels.get ys i) in
         (match !result with
         | Some arr -> ArrayLabels.set arr !index value
         | None -> result := Some (A.make l value));
         index := !index + 1
       done;
       match !result with
       | Some array -> array
       | None -> [||]
      : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array)

  let zip = (fun xs ys -> zip_with (fun a b -> a, b) xs ys : 'a array -> 'b array -> 'c array)

  module type EQ_F = functor (E : EQ) -> EQ with type t = E.t array

  module type ORD_F = functor (O : ORD) -> ORD with type t = O.t array

  module type SHOW_F = functor (S : SHOW) -> SHOW with type t = S.t array

  module type TRAVERSABLE_F = functor (A : APPLICATIVE) ->
    TRAVERSABLE with type 'a t = 'a array and type 'a applicative_t = 'a A.t

  module Functor : FUNCTOR with type 'a t = 'a array = struct
    type 'a t = 'a array

    let map = A.map
  end

  module Alt : ALT with type 'a t = 'a array = struct
    include Functor

    let alt = A.append
  end

  module Apply : APPLY with type 'a t = 'a array = struct
    include Functor

    let apply fn_array a = A.fold_left (fun acc f -> Alt.alt acc (map f a)) [||] fn_array
  end

  module Applicative : APPLICATIVE with type 'a t = 'a array = struct
    include Apply

    let pure a = [|a|]
  end

  module Monad : MONAD with type 'a t = 'a array = struct
    include Applicative

    let flat_map x f = A.fold_left (fun acc a -> Alt.alt acc (f a)) [||] x
  end

  module Foldable : FOLDABLE with type 'a t = 'a array = struct
    type 'a t = 'a array

    let fold_left = A.fold_left

    and fold_right f init = ArrayLabels.fold_right ~f ~init

    module Fold_Map (M : MONOID) = struct
      module D =
        Default.Fold_Map
          (M)
          (struct
            type 'a t = 'a array

            let fold_left, fold_right = fold_left, fold_right
          end)

      let fold_map = D.fold_map_default_left
    end

    module Fold_Map_Any (M : MONOID_ANY) = struct
      module D =
        Default.Fold_Map_Any
          (M)
          (struct
            type 'a t = 'a array

            let fold_left, fold_right = fold_left, fold_right
          end)

      let fold_map = D.fold_map_default_left
    end

    module Fold_Map_Plus (P : PLUS) = struct
      module D =
        Default.Fold_Map_Plus
          (P)
          (struct
            type 'a t = 'a array

            let fold_left, fold_right = fold_left, fold_right
          end)

      let fold_map = D.fold_map_default_left
    end
  end

  module Unfoldable : UNFOLDABLE with type 'a t = 'a array = struct
    type 'a t = 'a array

    let rec unfold f init =
      match f init with
      | Some (a, next) -> Alt.alt [|a|] (unfold f next)
      | None -> [||]
  end

  module Traversable : TRAVERSABLE_F =
  functor
    (A : APPLICATIVE)
    ->
    struct
      type 'a t = 'a array

      and 'a applicative_t = 'a A.t

      include (Functor : FUNCTOR with type 'a t := 'a t)

      include (Foldable : FOLDABLE with type 'a t := 'a t)

      module I = Infix.Apply (A)

      let traverse f =
        let open I in
        ArrayLabels.fold_right
          ~f:(fun acc x -> A.pure (fun x y -> Alt.alt [|x|] y) <*> f acc <*> x)
          ~init:(A.pure [||])

      module D = Default.Sequence (struct
        type 'a t = 'a array

        and 'a applicative_t = 'a A.t

        let traverse = traverse
      end)

      let sequence = D.sequence_default
    end

  module Eq : EQ_F =
  functor
    (E : EQ)
    ->
    struct
      type t = E.t array

      let eq xs ys = A.length xs = A.length ys && A.every (fun (a, b) -> E.eq a b) (zip xs ys)
    end

  module Ord : ORD_F =
  functor
    (O : ORD)
    ->
    struct
      include Eq (O)

      let compare xs ys =
        match xs, ys with
        | _ when A.length xs = A.length ys ->
            let index = ref 0 in
            A.fold_left
              (fun acc e ->
                let result =
                  match acc <> `equal_to with
                  | true -> acc
                  | false -> O.compare e (ArrayLabels.get ys !index)
                in
                index := !index + 1;
                result)
              `equal_to
              xs
        | _ when A.length xs < A.length ys -> `less_than
        | _ -> `greater_than
    end

  module Show : SHOW_F =
  functor
    (S : SHOW)
    ->
    struct
      module F = Functions.Foldable (Foldable)
      module M = F.Monoid (String.Monoid)

      type t = S.t array

      let show xs = "[" ^ M.intercalate ~separator:", " (Functor.map S.show xs) ^ "]"
    end

  module Invariant : INVARIANT with type 'a t = 'a array = struct
    type 'a t = 'a array

    let imap f _ = Functor.map f
  end

  module Extend : EXTEND with type 'a t = 'a array = struct
    include Functor

    let extend f xs = A.mapi (fun _ i -> f (A.slice xs ~start:i ~end_:(A.length xs))) xs
  end

  module Infix = struct
    include Infix.Monad (Monad)
    include Infix.Extend (Extend)
  end
end
