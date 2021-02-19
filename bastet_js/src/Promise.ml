open Interface

let ( <. ) = Function.Infix.( <. )

(** Note: Promises are not actually Monads because you can't have `'a Js.Promise.t Js.Promise.t`
    Even though it's a valid bucklescript signature. Promises auto-flatten in this case.
    See the unit tests. *)

module Functor : FUNCTOR with type 'a t = 'a Js.Promise.t = struct
  type 'a t = 'a Js.Promise.t

  let map f a = Js.Promise.then_ (Js.Promise.resolve <. f) a
end

module Apply : APPLY with type 'a t = 'a Js.Promise.t = struct
  include Functor

  let apply f a =
    Js.Promise.then_ (fun f' -> Js.Promise.then_ (fun a' -> Js.Promise.resolve (f' a')) a) f
end

module Applicative : APPLICATIVE with type 'a t = 'a Js.Promise.t = struct
  include Apply

  let pure = Js.Promise.resolve
end
