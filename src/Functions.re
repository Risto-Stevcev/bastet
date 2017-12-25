open Interface;

let (const, flip) = Function.((const, flip));
let (id) = Function.Category.(id);
let ((<<)) = Function.Infix.((<<));

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

  module Infix = {
    let (<*) = apply_first;
    let (*>) = apply_second;
  };
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

module Foldable = (F: FOLDABLE) => {
  module Semigroup = (S: SEMIGROUP) => {
    module FM = F.Fold_Map_Any(Endo.Monoid);
    module I = Infix.Semigroup(S);
    let surround_map: (~delimiter:S.t, 'a => S.t, F.t('a)) => S.t =
      (~delimiter, f, fa) => I.({
        let joined = (a) => Endo.Endo(m => delimiter <:> f(a) <:> m);
        let Endo.Endo(fn) = FM.fold_map(joined, fa);
        fn(delimiter)
      });

    let surround: (~delimiter:S.t, F.t('a)) => S.t =
      (~delimiter, fa) => surround_map(~delimiter, id, fa);
  };

  module Monoid = (M: MONOID) => {
    module FM = F.Fold_Map(M);
    module I = Infix.Monoid(M);
    type acc = { init: bool, acc: M.t };
    let fold: F.t(M.t) => M.t = FM.fold_map(id);
    let intercalate: (~separator:M.t, F.t(M.t)) => M.t =
      (~separator, xs) => {
        let go = (acc, x) => switch acc {
          | {init: true, acc: _} => { init: false, acc: x }
          | {init: _, acc: acc'} => I.{ init: false, acc: acc' <:> separator <:> x }
          };
        F.fold_left(go, {init: true, acc: M.empty}, xs).acc
      };
  };

  module Applicative = (A: APPLICATIVE) => {
    module Fn = Apply(A);
    let traverse': ('a => A.t('b), F.t('a)) => A.t(unit) =
      (f, fa) => F.fold_right(Fn.apply_second << f, A.pure(), fa);
    let sequence': F.t(A.t('a)) => A.t(unit) = (fa) => traverse'(id, fa);
  };

  module Plus = (P: PLUS) => {
    let one_of: (F.t(P.t('a))) => P.t('a) = (fa) => F.fold_right(P.alt, P.empty, fa);
  };

  module Monad = (M: MONAD) => {
    module I = Infix.Monad(M);
    let fold_monad: (('a, 'b) => M.t('a), 'a, F.t('b)) => M.t('a) =
      (f, a, fa) => I.(F.fold_left((acc, x) => acc >>= flip(f, x), M.pure(a), fa))
  };
};


module Traversable = (T: TRAVERSABLE_F) => {
  module Internal = {
    module type TYPE = {type s};
    type accum('s, 'a) = { accum: 's, value: 'a };
    type state('s, 'a) = 's => accum('s, 'a);
    let apply_state: (state('s, 'a), 's) => accum('s, 'a) = (s, a) => s(a);

    module State_Left = (Type: (module type of {type t})) => {
      module Functor: FUNCTOR with type t('a) = state(Type.t, 'a) = {
        type t('a) = state(Type.t, 'a);
        let map = (f, k) =>
          (s) =>
            switch (apply_state(k, s)) {
            | { accum: s1, value: a } => { accum: s1, value: f(a) }
            };
      };
      module Apply: APPLY with type t('a) = state(Type.t, 'a) = {
        include Functor;
        let apply = (f, x) =>
          (s) =>
            switch (apply_state(f, s)) {
            | { accum: s1, value: f' } =>
              switch (apply_state(x, s1)) {
              | { accum: s2, value: x' } => { accum: s2, value: f'(x') }
              }
            };
      };
      module Applicative: APPLICATIVE with type t('a) = state(Type.t, 'a) = {
        include Apply;
        let pure = (a) => (s) => { accum: s, value: a };
      };
    };

    module State_Right = (Type: (module type of {type t})) => {
      module Functor: FUNCTOR with type t('a) = state(Type.t, 'a) = {
        type t('a) = state(Type.t, 'a);
        let map = (f, k) =>
          (s) =>
            switch (apply_state(k, s)) {
            | { accum: s1, value: a } => { accum: s1, value: f(a) }
            };
      };
      module Apply: APPLY with type t('a) = state(Type.t, 'a) = {
        include Functor;
        let apply = (f, x) =>
          (s) =>
            switch (apply_state(x, s)) {
            | { accum: s1, value: x' } =>
              switch (apply_state(f, s1)) {
              | { accum: s2, value: f' } => { accum: s2, value: f'(x') }
              }
            };
      };
      module Applicative: APPLICATIVE with type t('a) = state(Type.t, 'a) = {
        include Apply;
        let pure = (a) => (s) => { accum: s, value: a };
      };
    };

    module Map_Accum = (Type: TYPE, T: TRAVERSABLE_F) => {
      module SL = State_Left({type t = Type.s});
      module SR = State_Right({type t = Type.s});
      module TSL = T(SL.Applicative);
      module TSR = T(SR.Applicative);
      let map_accum_left:
        (('s, 'a) => accum('s, 'b), 's, TSL.t('a)) => accum('s, TSL.t('b)) =
        (f, s, xs) => {
          apply_state(TSL.traverse((a) => ((s') => f(s', a)), xs))(s);
        };
      let map_accum_right:
        (('s, 'a) => accum('s, 'b), 's, TSR.t('a)) => accum('s, TSR.t('b)) =
        (f, s, xs) => {
          apply_state(TSR.traverse((a) => ((s') => f(s', a)), xs))(s);
        }
    };
  };

  module Scan = (Type: Internal.TYPE) => {
    module MA = Internal.Map_Accum({type s = Type.s}, T);
    let scan_left: (('b, 'a) => 'b, 'b, MA.TSL.t('a)) => MA.TSL.t('b) =
      (f, init, xs) =>
        MA.map_accum_left((b, a) => {
          let b' = f(b, a);
          { accum: b', value: b' }
        }, init, xs).value;

    let scan_right: (('a, 'b) => 'b, 'b, MA.TSR.t('a)) => MA.TSR.t('b) =
      (f, init, xs) =>
        MA.map_accum_right((b, a) => {
          let b' = f(a, b);
          { accum: b', value: b' }
        }, init, xs).value;
  };
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
