open Interface

let ( <. ) = Function.Infix.( <. )

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

module Monad : MONAD with type 'a t = 'a Js.Promise.t = struct
  include Applicative

  let flat_map a f = Js.Promise.then_ f a
end

module Infix = struct
  include Infix.Monad (Monad)
end
