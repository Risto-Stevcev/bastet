open Interface

let flip, const =
  let open Function in
  flip, const

let result =
  (fun f g a ->
     match f, g, a with
     | f, _, Ok a' -> f a'
     | _, g, Error a' -> g a'
    : ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) result -> 'c)

module type MAGMA_F = functor (T : TYPE) (M : MAGMA) -> MAGMA with type t = (M.t, T.t) result

module type MEDIAL_MAGMA_F = functor (T : TYPE) (M : MAGMA) ->
  MEDIAL_MAGMA with type t = (M.t, T.t) result

module type SEMIGROUP_F = functor (T : TYPE) (S : SEMIGROUP) ->
  SEMIGROUP with type t = (S.t, T.t) result

module type FUNCTOR_F = functor (T : TYPE) -> FUNCTOR with type 'a t = ('a, T.t) result

module type APPLY_F = functor (T : TYPE) -> APPLY with type 'a t = ('a, T.t) result

module type APPLICATIVE_F = functor (T : TYPE) -> APPLICATIVE with type 'a t = ('a, T.t) result

module type MONAD_F = functor (T : TYPE) -> MONAD with type 'a t = ('a, T.t) result

module type ALT_F = functor (T : TYPE) -> ALT with type 'a t = ('a, T.t) result

module type EXTEND_F = functor (T : TYPE) -> EXTEND with type 'a t = ('a, T.t) result

module type SHOW_F = functor (Ok : SHOW) (Error : SHOW) -> SHOW with type t = (Ok.t, Error.t) result

module type EQ_F = functor (Ok : EQ) (Error : EQ) -> EQ with type t = (Ok.t, Error.t) result

module type ORD_F = functor (Ok : ORD) (Error : ORD) -> ORD with type t = (Ok.t, Error.t) result

module type BOUNDED_F = functor (Ok : BOUNDED) (Error : BOUNDED) ->
  BOUNDED with type t = (Ok.t, Error.t) result

module type FOLDABLE_F = functor (T : TYPE) -> FOLDABLE with type 'a t = ('a, T.t) result

module type TRAVERSABLE_F = functor (T : TYPE) (A : APPLICATIVE) ->
  TRAVERSABLE with type 'a t = ('a, T.t) result and type 'a applicative_t = 'a A.t

module type BITRAVERSABLE_F = functor (A : APPLICATIVE) ->
  BITRAVERSABLE with type ('a, 'b) t = ('a, 'b) result and type 'a applicative_t = 'a A.t

module Magma : MAGMA_F =
functor
  (T : TYPE)
  (M : MAGMA)
  ->
  struct
    type t = (M.t, T.t) result

    let append a b =
      match a, b with
      | Ok a', Ok b' -> Ok (M.append a' b')
      | _, Ok b' -> Ok b'
      | Ok a', _ -> Ok a'
      | Error a', _ -> Error a'
  end

module Medial_Magma : MEDIAL_MAGMA_F =
functor
  (T : TYPE)
  (M : MAGMA)
  ->
  struct
    include Magma (T) (M)
  end

module Semigroup : SEMIGROUP_F =
functor
  (T : TYPE)
  (S : SEMIGROUP)
  ->
  struct
    include Magma (T) (S)
  end

module Functor : FUNCTOR_F =
functor
  (T : TYPE)
  ->
  struct
    type 'a t = ('a, T.t) result

    let map f a =
      match a with
      | Ok r -> Ok (f r)
      | Error l -> Error l
  end

module Bifunctor : BIFUNCTOR with type ('a, 'b) t = ('a, 'b) result = struct
  type ('a, 'b) t = ('a, 'b) result

  let bimap f g a =
    match a with
    | Ok a' -> Ok (f a')
    | Error a' -> Error (g a')
end

module Apply : APPLY_F =
functor
  (T : TYPE)
  ->
  struct
    include Functor (T)

    let apply f a =
      match f, a with
      | Ok f', a' -> map f' a'
      | Error f', _ -> Error f'
  end

module Applicative : APPLICATIVE_F =
functor
  (T : TYPE)
  ->
  struct
    include Apply (T)

    let pure a = Ok a
  end

module Monad : MONAD_F =
functor
  (T : TYPE)
  ->
  struct
    include Applicative (T)

    let flat_map a f =
      match a with
      | Ok a' -> f a'
      | Error a' -> Error a'
  end

module Alt : ALT_F =
functor
  (T : TYPE)
  ->
  struct
    include Functor (T)

    let alt a b =
      match a, b with
      | Error _, b' -> b'
      | a', _ -> a'
  end

module Extend : EXTEND_F =
functor
  (T : TYPE)
  ->
  struct
    include Monad (T)

    let extend f a =
      match f, a with
      | _, Error a' -> Error a'
      | f', a' -> Ok (f' a')
  end

module Show : SHOW_F =
functor
  (Ok : SHOW)
  (Error : SHOW)
  ->
  struct
    type t = (Ok.t, Error.t) result

    let show = result Ok.show Error.show
  end

module Eq : EQ_F =
functor
  (Ok : EQ)
  (Error : EQ)
  ->
  struct
    type t = (Ok.t, Error.t) result

    let eq a b =
      match a, b with
      | Ok a', Ok b' -> Ok.eq a' b'
      | Error a', Error b' -> Error.eq a' b'
      | _ -> false
  end

module Ord : ORD_F =
functor
  (Ok : ORD)
  (Error : ORD)
  ->
  struct
    include Eq (Ok) (Error)

    let compare a b =
      match a, b with
      | Ok a', Ok b' -> Ok.compare a' b'
      | Error a', Error b' -> Error.compare a' b'
      | Error _, Ok _ -> `less_than
      | Ok _, Error _ -> `greater_than
  end

module Bounded : BOUNDED_F =
functor
  (Ok : BOUNDED)
  (Error : BOUNDED)
  ->
  struct
    include Ord (Ok) (Error)

    let top = Ok Ok.top

    let bottom = Error Error.bottom
  end

module Many_Valued_Logic = struct
  module type EQ_F = functor (Ok : TYPE) (Error : TYPE) -> EQ with type t = (Ok.t, Error.t) result

  module type ORD_F = functor (Ok : TYPE) (Error : TYPE) -> ORD with type t = (Ok.t, Error.t) result

  module type JOIN_SEMILATTICE_F = functor (Ok : JOIN_SEMILATTICE) (Error : JOIN_SEMILATTICE) ->
    JOIN_SEMILATTICE with type t = (Ok.t, Error.t) result

  module type MEET_SEMILATTICE_F = functor (Ok : MEET_SEMILATTICE) (Error : MEET_SEMILATTICE) ->
    MEET_SEMILATTICE with type t = (Ok.t, Error.t) result

  module type BOUNDED_JOIN_SEMILATTICE_F = functor
    (Ok : BOUNDED_JOIN_SEMILATTICE)
    (Error : BOUNDED_JOIN_SEMILATTICE)
    -> BOUNDED_JOIN_SEMILATTICE with type t = (Ok.t, Error.t) result

  module type BOUNDED_MEET_SEMILATTICE_F = functor
    (Ok : BOUNDED_MEET_SEMILATTICE)
    (Error : BOUNDED_MEET_SEMILATTICE)
    -> BOUNDED_MEET_SEMILATTICE with type t = (Ok.t, Error.t) result

  module type HEYTING_ALGEBRA_F = functor (Ok : HEYTING_ALGEBRA) (Error : HEYTING_ALGEBRA) ->
    HEYTING_ALGEBRA with type t = (Ok.t, Error.t) result

  module Quasireflexive_Eq : EQ_F =
  functor
    (Ok : TYPE)
    (Error : TYPE)
    ->
    struct
      type t = (Ok.t, Error.t) result

      let eq a b =
        match a, b with
        | Ok _, Ok _ | Error _, Error _ -> true
        | _ -> false
    end

  module Quasireflexive_Ord : ORD_F =
  functor
    (Ok : TYPE)
    (Error : TYPE)
    ->
    struct
      include Quasireflexive_Eq (Ok) (Error)

      let compare a b =
        match a, b with
        | Ok _, Ok _ | Error _, Error _ -> `equal_to
        | Error _, Ok _ -> `less_than
        | Ok _, Error _ -> `greater_than
    end

  module Join_Semilattice : JOIN_SEMILATTICE_F =
  functor
    (Ok : JOIN_SEMILATTICE)
    (Error : JOIN_SEMILATTICE)
    ->
    struct
      type t = (Ok.t, Error.t) result

      let join a b =
        match a, b with
        | Ok a', Ok b' -> Ok (Ok.join a' b')
        | Ok a', _ | _, Ok a' -> Ok a'
        | Error a', Error b' -> Error (Error.join a' b')
    end

  module Meet_Semilattice : MEET_SEMILATTICE_F =
  functor
    (Ok : MEET_SEMILATTICE)
    (Error : MEET_SEMILATTICE)
    ->
    struct
      type t = (Ok.t, Error.t) result

      let meet a b =
        match a, b with
        | Ok a', Ok b' -> Ok (Ok.meet a' b')
        | Error a', Error b' -> Error (Error.meet a' b')
        | Error a', _ | _, Error a' -> Error a'
    end

  module Bounded_Join_Semilattice : BOUNDED_JOIN_SEMILATTICE_F =
  functor
    (Ok : BOUNDED_JOIN_SEMILATTICE)
    (Error : BOUNDED_JOIN_SEMILATTICE)
    ->
    struct
      include Join_Semilattice (Ok) (Error)

      let bottom = Error Error.bottom
    end

  module Bounded_Meet_Semilattice : BOUNDED_MEET_SEMILATTICE_F =
  functor
    (Ok : BOUNDED_MEET_SEMILATTICE)
    (Error : BOUNDED_MEET_SEMILATTICE)
    ->
    struct
      include Meet_Semilattice (Ok) (Error)

      let top = Ok Ok.top
    end

  module Lattice (Ok : LATTICE) (Error : LATTICE) = struct
    include Join_Semilattice (Ok) (Error)

    include (
      Meet_Semilattice (Ok) (Error) : module type of Meet_Semilattice (Ok) (Error) with type t := t)
    end

  module Bounded_Lattice (Ok : BOUNDED_LATTICE) (Error : BOUNDED_LATTICE) = struct
    include Bounded_Join_Semilattice (Ok) (Error)

    include (
      Bounded_Meet_Semilattice (Ok) (Error) :
          module type of Bounded_Meet_Semilattice (Ok) (Error) with type t := t)
    end

  module Distributive_Lattice (Ok : LATTICE) (Error : LATTICE) = struct
    include Lattice (Ok) (Error)
  end

  module Bounded_Distributive_Lattice (Ok : BOUNDED_LATTICE) (Error : BOUNDED_LATTICE) = struct
    include Bounded_Lattice (Ok) (Error)
  end

  module Heyting_Algebra : HEYTING_ALGEBRA_F =
  functor
    (Ok : HEYTING_ALGEBRA)
    (Error : HEYTING_ALGEBRA)
    ->
    struct
      include Quasireflexive_Ord (Ok) (Error)

      include (
        Bounded_Distributive_Lattice (Ok) (Error) :
            module type of Bounded_Distributive_Lattice (Ok) (Error) with type t := t)

        let not a =
          match a with
          | Ok a' when a' = Ok.top -> Error Error.bottom
          | Ok a' when a' = Ok.bottom -> Error Error.top
          | Error a' when a' = Error.top -> Ok Ok.bottom
          | Error a' when a' = Error.bottom -> Ok Ok.top
          | a' -> a'

        let implies a b = join (not a) b
      end

  module Involutive_Heyting_Algebra
      (Ok : INVOLUTIVE_HEYTING_ALGEBRA)
      (Error : INVOLUTIVE_HEYTING_ALGEBRA) =
  struct
    include Heyting_Algebra (Ok) (Error)
  end

  module Boolean_Algebra (Ok : BOOLEAN_ALGEBRA) (Error : BOOLEAN_ALGEBRA) = struct
    include Heyting_Algebra (Ok) (Error)
  end
  end

  module Foldable : FOLDABLE_F =
  functor
    (T : TYPE)
    ->
    struct
      type 'a t = ('a, T.t) result

      let fold_left f initial a =
        match a with
        | Ok a' -> f initial a'
        | Error _ -> initial

      and fold_right f initial a =
        match a with
        | Ok a' -> f a' initial
        | Error _ -> initial

      module Fold_Map (M : MONOID) = struct
        let fold_map f a =
          match a with
          | Ok a' -> f a'
          | Error _ -> M.empty
      end

      module Fold_Map_Plus (P : PLUS) = struct
        let fold_map f a =
          match a with
          | Ok a' -> f a'
          | Error _ -> P.empty
      end

      module Fold_Map_Any (M : MONOID_ANY) = struct
        let fold_map f a =
          match a with
          | Ok a' -> f a'
          | Error _ -> M.empty
      end
    end

  module Bifoldable : BIFOLDABLE with type ('a, 'b) t = ('a, 'b) result = struct
    type ('a, 'b) t = ('a, 'b) result

    let bifold_left f g initial a =
      match a with
      | Ok a' -> f initial a'
      | Error a' -> g initial a'

    and bifold_right f g initial a =
      match a with
      | Ok a' -> f a' initial
      | Error a' -> g a' initial

    module Fold_Map (M : MONOID) = struct
      let fold_map = result
    end

    module Fold_Map_Any (M : MONOID_ANY) = struct
      let fold_map = result
    end

    module Fold_Map_Plus (P : PLUS) = struct
      let fold_map = result
    end
  end

  module Traversable : TRAVERSABLE_F =
  functor
    (T : TYPE)
    (A : APPLICATIVE)
    ->
    struct
      module E = Applicative (T)

      type 'a t = ('a, T.t) result

      and 'a applicative_t = 'a A.t

      include (Functor (T) : FUNCTOR with type 'a t := 'a t)

      include (Foldable (T) : FOLDABLE with type 'a t := 'a t)

      let traverse f a =
        match a with
        | Ok a' -> A.map E.pure (f a')
        | Error a' -> A.pure (Error a')

      and sequence a =
        match a with
        | Ok a' -> A.map E.pure a'
        | Error a' -> A.pure (Error a')
    end

  module Bitraversable : BITRAVERSABLE_F =
  functor
    (A : APPLICATIVE)
    ->
    struct
      type ('a, 'b) t = ('a, 'b) result

      and 'a applicative_t = 'a A.t

      include (Bifunctor : BIFUNCTOR with type ('a, 'b) t := ('a, 'b) t)

      include (Bifoldable : BIFOLDABLE with type ('a, 'b) t := ('a, 'b) t)

      let bitraverse f g a =
        match a with
        | Ok a' -> A.map (fun x -> Ok x) (f a')
        | Error a' -> A.map (fun x -> Error x) (g a')

      and bisequence a =
        match a with
        | Ok a' -> A.map (fun x -> Ok x) a'
        | Error a' -> A.map (fun x -> Error x) a'
    end

  module Infix = struct
    include Infix.Bifunctor (Bifunctor)
  end

  module Choose (A : ALT) = struct
    let choose =
      (fun a b -> A.alt (A.map (fun x -> Ok x) a) (A.map (fun x -> Error x) b)
        : 'a A.t -> 'b A.t -> ('a, 'b) result A.t)
  end

  module Unsafe = struct
    let from_ok a =
      match a with
      | Ok a' -> a'
      | _ -> raise (Invalid_argument "You passed in an `Error` value to `from_ok`")

    and from_error a =
      match a with
      | Error a' -> a'
      | _ -> raise (Invalid_argument "You passed in an `Ok` value to `from_error`")
  end

  let is_ok a = result (const true) (const false) a

  and is_error a = result (const false) (const true) a

  and note =
    (fun default -> Option.maybe ~f:(fun x -> Ok x) ~default:(Error default)
      : 'err -> 'a option -> ('a, 'err) result)

  and hush =
    (fun e -> result Option.Applicative.pure (const None) e : ('a, 'err) result -> 'a option)
