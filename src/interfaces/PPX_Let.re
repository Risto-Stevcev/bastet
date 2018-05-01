/* This is a helper module to integrate `bs-abstract` with `ppx_let` */
open Interface;

/* The module structure that `ppx_let` expects to be in scope */
module type PPX_LET = {
  type t('a);
  let return: 'a => t('a);
  let bind: (t('a), 'a => t('b)) => t('b);
  let map: (t('a), ~f: 'a => 'b) => t('b);
  let both: (t('a), t('b)) => t(('a, 'b));
  module Open_on_rhs: { let return: 'a => t('a) }
};

/* Makes the `ppx_let` module from a monad */
module Make = (M: MONAD) => {
  module Let_syntax: PPX_LET with type t('a) = M.t('a) = {
    type t('a) = M.t('a);

    let return = M.pure
    and bind = M.flat_map
    and map = (a, ~f) => M.map(f, a)
    and both = (a, b) =>
      M.flat_map(a, a' => {
        M.flat_map(b, b' => {
          M.pure((a', b'))
        })
      });

    module Open_on_rhs = { let return = M.pure }
  };
};
