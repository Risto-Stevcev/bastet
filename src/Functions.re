open Interface;

let (const) = Function.(const);
let (id) = Function.Category.(id);

module Monoid = (M : MONOID) => {
  module I = Infix.Monoid(M);
  let power: (M.t, int) => M.t = (x, p) => I.({
    let rec go = (p) => switch p {
      | p when p <= 0 => M.empty
      | p when p == 1 => x
      | p when (p mod 2) == 0 => {
          let x' = go(p / 2);
          x' <:> x'
        }
      | _ => {
          let x' = go(p / 2);
          x' <:> x' <:> x
        }
      };
    go(p);
  });
  let guard: (bool, M.t) => M.t = (p, a) => p ? a : M.empty;
};

module Functor = (F: FUNCTOR) => {
  let void: F.t('a) => F.t(unit) = (fa) => F.map(const(), fa);
  let void_right: ('a, F.t('b)) => F.t('a) = (a, fb) => F.map(const(a), fb);
  let void_left: (F.t('a), 'b) => F.t('b) = (fa, b) => F.map(const(b), fa);
  let flap: (F.t(('a => 'b)), 'a) => F.t('b) = (fs, a) => F.map((f) => f(a), fs);
};

module Apply = (A: APPLY) => {
  module I = {
    include Infix.Apply(A);
    include (Infix.Functor(A): (module type of Infix.Functor(A)) with type t('a) := t('a))
  };
  open I;

  let apply_first: (A.t('a), A.t('b)) => A.t('a) = (a, b) => const <$> a <*> b;
  let apply_second: (A.t('a), A.t('b)) => A.t('b) = (a, b) => const(id) <$> a <*> b;

  let lift2: (('a, 'b) => 'c, A.t('a), A.t('b)) => A.t('c) =
    (f, a, b) => f <$> a <*> b;
  let lift3: (('a, 'b, 'c) => 'd, A.t('a), A.t('b), A.t('c)) => A.t('d) =
    (f, a, b, c) => f <$> a <*> b <*> c;
  let lift4: (('a, 'b, 'c, 'd) => 'e, A.t('a), A.t('b), A.t('c), A.t('d)) => A.t('e) =
    (f, a, b, c, d) => f <$> a <*> b <*> c <*> d;
  let lift5:
    (('a, 'b, 'c, 'd, 'e) => 'f, A.t('a), A.t('b), A.t('c), A.t('d), A.t('e)) => A.t('f) =
    (f, a, b, c, d, e) => f <$> a <*> b <*> c <*> d <*> e;
};

module Applicative = (A: APPLICATIVE) => {
  module I = Infix.Applicative(A);
  let liftA1: (('a => 'b), A.t('a)) => A.t('b) = (f, fa) => I.(pure(f) <*> fa);
  let when_: (bool, A.t(unit)) => A.t(unit) = (p, fa) => p ? fa : A.pure();
  let unless: (bool, A.t(unit)) => A.t(unit) = (p, fa) => !p ? fa : A.pure();
};

module Monad = (M: MONAD) => {
  module I = Infix.Monad(M);
  module A = Applicative(M);

  let flatten: M.t(M.t('a)) => M.t('a) = (m) => I.(m >>= id);
  let compose_kliesli: ('a => M.t('b), 'b => M.t('c)) => ('a => M.t('c)) =
    (f, g, a) => I.(f(a) >>= g);
  let compose_kliesli_flipped: ('b => M.t('c), 'a => M.t('b)) => ('a => M.t('c)) =
    (f, g, a) => I.(f =<< g(a));
  let if_m: (M.t(bool), M.t('a), M.t('a)) => M.t('a) =
    (p, t, f) => I.(p >>= (p') => p' ? t : f);
  let liftM1: (('a => 'b), M.t('a)) => M.t('b) =
    (f, fa) => I.(fa >>= (fa') => pure(f(fa')));
  let ap: (M.t('a => 'b), M.t('a)) => M.t('b) =
    (f, fa) => I.({
      f >>= (f') =>
      fa >>= (fa') =>
      M.pure(f'(fa'))
    });
  let when_: (M.t(bool), M.t(unit)) => M.t(unit) =
    (p, fa) => I.(p >>= (p') => A.when_(p', fa));
  let unless: (M.t(bool), M.t(unit)) => M.t(unit) =
    (p, fa) => I.(p >>= (p') => A.unless(p', fa));
};

module Infix = {
  module Apply = (A: APPLY) => {
    module Functions = Apply(A);
    let (<*) = Functions.apply_first;
    let (*>) = Functions.apply_second;
  };
  module Monad = (M: MONAD) => {
    module Functions = Monad(M);
    let (<=<) = Functions.compose_kliesli;
    let (>=>) = Functions.compose_kliesli_flipped;
  };
  module Void = (F: FUNCTOR) => {
    module Functions = Functor(F);
    let ($>) = Functions.void_left;
    let (<$) = Functions.void_right;
    let (<@>) = Functions.flap;
  };
};
