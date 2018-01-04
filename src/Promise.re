open Interface;
let (<.) = Function.Infix.(<.);

/* Note: Promises are not actually Monads because you can't have
 * Js.Promise.t(Js.Promise.t('a))
 * Even though it's a valid bucklescript signature. See the unit tests.
 */

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

/* It's is NOT a Monad, this is just here for convenience */
module Monad: MONAD with type t('a) = Js.Promise.t('a) = {
  include Applicative;
  let flat_map = (a, f) => Js.Promise.then_(f, a);
};

module Infix = {
  include Infix.Monad(Monad);
};
