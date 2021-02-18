open Interface

let const, flip =
  let open Function in
  const, flip

and id =
  let open Function.Category in
  id

and ( <. ) = Function.Infix.( <. )

module Monoid (M : MONOID) = struct
  module I = Infix.Magma (M)

  let power =
    (fun x p ->
       let open I in
       let rec go p =
         match p with
         | p when p <= 0 -> M.empty
         | p when p = 1 -> x
         | p when p mod 2 = 0 ->
             let x' = go (p / 2) in
             x' <:> x'
         | _ ->
             let x' = go (p / 2) in
             x' <:> x' <:> x
       in
       go p
      : M.t -> int -> M.t)

  and guard =
    (fun p a ->
       match p with
       | true -> a
       | false -> M.empty
      : bool -> M.t -> M.t)
end

module Functor (F : FUNCTOR) = struct
  let void = (fun fa -> F.map (const ()) fa : 'a F.t -> unit F.t)

  and void_right = (fun a fb -> F.map (const a) fb : 'a -> 'b F.t -> 'a F.t)

  and void_left = (fun fa b -> F.map (const b) fa : 'a F.t -> 'b -> 'b F.t)

  and flap = (fun fs a -> F.map (fun f -> f a) fs : ('a -> 'b) F.t -> 'a -> 'b F.t)
end

module Apply (A : APPLY) = struct
  module I = Infix.Apply (A)
  open I

  let apply_first = (fun a b -> const <$> a <*> b : 'a A.t -> 'b A.t -> 'a A.t)

  and apply_second = (fun a b -> const id <$> a <*> b : 'a A.t -> 'b A.t -> 'b A.t)

  and apply_both =
    (fun a b -> (fun a' b' -> a', b') <$> a <*> b : 'a A.t -> 'b A.t -> ('a * 'b) A.t)

  and lift2 = (fun f a b -> f <$> a <*> b : ('a -> 'b -> 'c) -> 'a A.t -> 'b A.t -> 'c A.t)

  and lift3 =
    (fun f a b c -> f <$> a <*> b <*> c
      : ('a -> 'b -> 'c -> 'd) -> 'a A.t -> 'b A.t -> 'c A.t -> 'd A.t)

  and lift4 =
    (fun f a b c d -> f <$> a <*> b <*> c <*> d
      : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a A.t -> 'b A.t -> 'c A.t -> 'd A.t -> 'e A.t)

  and lift5 =
    (fun f a b c d e -> f <$> a <*> b <*> c <*> d <*> e
      : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
        'a A.t ->
        'b A.t ->
        'c A.t ->
        'd A.t ->
        'e A.t ->
        'f A.t)

  module Infix = struct
    let ( <* ) = apply_first

    and ( *> ) = apply_second
  end
end

module Apply' (A : APPLY) (T : TYPE) = struct
  module F = Function.Apply (struct
    type t = T.t
  end)

  module F' = Function.Apply (struct
    type t = T.t A.t
  end)

  module Apply_F = Apply (F)
  module Apply_A = Apply (A)

  let apply_const =
    (fun f x -> F'.apply Apply_A.apply_first f x : (T.t A.t -> 'a A.t) -> T.t A.t -> T.t A.t)

  let apply_first =
    (fun f g x -> Apply_F.lift2 Apply_A.apply_first f g x
      : (T.t -> 'a A.t) -> (T.t -> 'b A.t) -> T.t -> 'a A.t)

  and apply_second =
    (fun f g x -> Apply_F.lift2 Apply_A.apply_second f g x
      : (T.t -> 'a A.t) -> (T.t -> 'b A.t) -> T.t -> 'b A.t)

  and apply_both =
    (fun f g x -> Apply_F.lift2 Apply_A.apply_both f g x
      : (T.t -> 'a A.t) -> (T.t -> 'b A.t) -> T.t -> ('a * 'b) A.t)
end

module Applicative (A : APPLICATIVE) = struct
  module I = Infix.Apply (A)

  let liftA1 =
    (fun f fa ->
       let open I in
       A.pure f <*> fa
      : ('a -> 'b) -> 'a A.t -> 'b A.t)

  and when_ =
    (fun p fa ->
       match p with
       | true -> fa
       | false -> A.pure ()
      : bool -> unit A.t -> unit A.t)

  and unless =
    (fun p fa ->
       match not p with
       | true -> fa
       | false -> A.pure ()
      : bool -> unit A.t -> unit A.t)
end

module Monad (M : MONAD) = struct
  module I = Infix.Monad (M)
  module A = Applicative (M)

  let flatten =
    (fun m ->
       let open I in
       m >>= id
      : 'a M.t M.t -> 'a M.t)

  and compose_kliesli =
    (fun f g a ->
       let open I in
       f a >>= g
      : ('a -> 'b M.t) -> ('b -> 'c M.t) -> 'a -> 'c M.t)

  and compose_kliesli_flipped =
    (fun f g a ->
       let open I in
       f =<< g a
      : ('b -> 'c M.t) -> ('a -> 'b M.t) -> 'a -> 'c M.t)

  and if_m =
    (fun p t f ->
       let open I in
       p >>= fun p' ->
       match p' with
       | true -> t
       | false -> f
      : bool M.t -> 'a M.t -> 'a M.t -> 'a M.t)

  and liftM1 =
    (fun f fa ->
       let open I in
       fa >>= fun fa' -> M.pure (f fa')
      : ('a -> 'b) -> 'a M.t -> 'b M.t)

  and ap =
    (fun f fa ->
       let open I in
       f >>= fun f' ->
       fa >>= fun fa' -> M.pure (f' fa')
      : ('a -> 'b) M.t -> 'a M.t -> 'b M.t)

  and when_ =
    (fun p fa ->
       let open I in
       p >>= fun p' -> A.when_ p' fa
      : bool M.t -> unit M.t -> unit M.t)

  and unless =
    (fun p fa ->
       let open I in
       p >>= fun p' -> A.unless p' fa
      : bool M.t -> unit M.t -> unit M.t)
end

module Foldable (F : FOLDABLE) = struct
  module Semigroup (S : SEMIGROUP) = struct
    module FM = F.Fold_Map_Any (Endo.Monoid)
    module I = Infix.Magma (S)

    let surround_map =
      (fun ~delimiter f fa ->
         let open I in
         let joined a = Endo.Endo (fun m -> delimiter <:> f a <:> m) in
         let (Endo.Endo fn) = FM.fold_map joined fa in
         fn delimiter
        : delimiter:S.t -> ('a -> S.t) -> 'a F.t -> S.t)

    let surround =
      (fun ~delimiter fa -> surround_map ~delimiter id fa : delimiter:S.t -> 'a F.t -> S.t)
  end

  module Monoid (M : MONOID) = struct
    module FM = F.Fold_Map (M)
    module I = Infix.Magma (M)

    type acc = {
      init : bool;
      acc : M.t;
    }

    let fold = (FM.fold_map id : M.t F.t -> M.t)

    and intercalate =
      (fun ~separator xs ->
         let go acc x =
           match acc with
           | { init = true; acc = _ } -> { init = false; acc = x }
           | { init = _; acc = acc' } ->
               let open I in
               { init = false; acc = acc' <:> separator <:> x }
         in
         (F.fold_left go { init = true; acc = M.empty } xs).acc
        : separator:M.t -> M.t F.t -> M.t)
  end

  module Applicative (A : APPLICATIVE) = struct
    module Fn = Apply (A)

    let traverse' =
      (fun f fa -> F.fold_right (Fn.apply_second <. f) (A.pure ()) fa
        : ('a -> 'b A.t) -> 'a F.t -> unit A.t)

    let sequence' = (fun fa -> traverse' id fa : 'a A.t F.t -> unit A.t)
  end

  module Plus (P : PLUS) = struct
    let one_of = (fun fa -> F.fold_right P.alt P.empty fa : 'a P.t F.t -> 'a P.t)
  end

  module Monad (M : MONAD) = struct
    module I = Infix.Monad (M)

    let fold_monad =
      (fun f a fa ->
         let open I in
         F.fold_left (fun acc x -> acc >>= flip f x) (M.pure a) fa
        : ('a -> 'b -> 'a M.t) -> 'a -> 'b F.t -> 'a M.t)
  end
end

module Traversable (T : TRAVERSABLE_F) = struct
  module Internal = struct
    type ('s, 'a) accum = {
      accum : 's;
      value : 'a;
    }

    type ('s, 'a) state = 's -> ('s, 'a) accum

    let apply_state = (fun s a -> s a : ('s, 'a) state -> 's -> ('s, 'a) accum)

    module State_Left (Type : TYPE) = struct
      module Functor : FUNCTOR with type 'a t = (Type.t, 'a) state = struct
        type 'a t = (Type.t, 'a) state

        let map f k s =
          match apply_state k s with
          | { accum = s1; value = a } -> { accum = s1; value = f a }
      end

      module Apply : APPLY with type 'a t = (Type.t, 'a) state = struct
        include Functor

        let apply f x s =
          match apply_state f s with
          | { accum = s1; value = f' } -> (
              match apply_state x s1 with
              | { accum = s2; value = x' } -> { accum = s2; value = f' x' })
      end

      module Applicative : APPLICATIVE with type 'a t = (Type.t, 'a) state = struct
        include Apply

        let pure a s = { accum = s; value = a }
      end
    end

    module State_Right (Type : TYPE) = struct
      module Functor : FUNCTOR with type 'a t = (Type.t, 'a) state = struct
        type 'a t = (Type.t, 'a) state

        let map f k s =
          match apply_state k s with
          | { accum = s1; value = a } -> { accum = s1; value = f a }
      end

      module Apply : APPLY with type 'a t = (Type.t, 'a) state = struct
        include Functor

        let apply f x s =
          match apply_state x s with
          | { accum = s1; value = x' } -> (
              match apply_state f s1 with
              | { accum = s2; value = f' } -> { accum = s2; value = f' x' })
      end

      module Applicative : APPLICATIVE with type 'a t = (Type.t, 'a) state = struct
        include Apply

        let pure a s = { accum = s; value = a }
      end
    end

    module Map_Accum (Type : TYPE) (T : TRAVERSABLE_F) = struct
      module SL = State_Left (struct
        type t = Type.t
      end)

      module SR = State_Right (struct
        type t = Type.t
      end)

      module TSL = T (SL.Applicative)
      module TSR = T (SR.Applicative)

      let map_accum_left =
        (fun f s xs -> apply_state (TSL.traverse (fun a s' -> f s' a) xs) s
          : ('s -> 'a -> ('s, 'b) accum) -> 's -> 'a TSL.t -> ('s, 'b TSL.t) accum)

      and map_accum_right =
        (fun f s xs -> apply_state (TSR.traverse (fun a s' -> f s' a) xs) s
          : ('s -> 'a -> ('s, 'b) accum) -> 's -> 'a TSR.t -> ('s, 'b TSR.t) accum)
    end
  end

  module Scan (Type : TYPE) = struct
    module MA =
      Internal.Map_Accum
        (struct
          type t = Type.t
        end)
        (T)

    let scan_left =
      (fun f init xs ->
         (MA.map_accum_left
            (fun b a ->
              let b' = f b a in
              { accum = b'; value = b' })
            init
            xs)
           .value
        : ('b -> 'a -> 'b) -> 'b -> 'a MA.TSL.t -> 'b MA.TSL.t)

    and scan_right =
      (fun f init xs ->
         (MA.map_accum_right
            (fun b a ->
              let b' = f a b in
              { accum = b'; value = b' })
            init
            xs)
           .value
        : ('a -> 'b -> 'b) -> 'b -> 'a MA.TSR.t -> 'b MA.TSR.t)
  end
end

module Infix = struct
  module Apply (A : APPLY) = struct
    module Functions = Apply (A)

    let ( <* ) = Functions.apply_first

    and ( *> ) = Functions.apply_second
  end

  module Monad (M : MONAD) = struct
    module Functions = Infix.Monad (M)

    let ( >=> ), ( <=< ) =
      let open Functions in
      ( >=> ), ( <=< )
  end

  module Void (F : FUNCTOR) = struct
    module Functions = Functor (F)

    let ( $> ) = Functions.void_left

    and ( <$ ) = Functions.void_right

    and ( <@> ) = Functions.flap
  end
end
