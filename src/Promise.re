open Interface;
let (<.) = Function.Infix.(<.);


module Functor: FUNCTOR with type t('a) = Js.Promise.t('a) = {
  type t('a) = Js.Promise.t('a);
  let map = (f, a) => Js.Promise.then_(Js.Promise.resolve <. f, a);
};

module Apply: APPLY with type t('a) = Js.Promise.t('a) = {
  include Functor;
  let apply = (f, a) =>
    Js.Promise.then_((f') => {
      Js.Promise.then_((a') => Js.Promise.resolve(f'(a')), a);
    }, f);
};

module Applicative: APPLICATIVE with type t('a) = Js.Promise.t('a) = {
  include Apply;
  let pure = Js.Promise.resolve;
};

module Monad: MONAD with type t('a) = Js.Promise.t('a) = {
  include Applicative;
  let flat_map = (a, f) => Js.Promise.then_(f, a);
};

module Infix = {
  include Infix.Monad(Monad);
};
