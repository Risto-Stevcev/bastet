open Interface;
let (<.) = Function.Infix.(<.);

%raw
{|
var fold_left = function(f, initial, a) {
  return Object.keys(a).reduce(function(acc, key) {
    return f(acc, a[key])
  }, initial)
};

var fold_left_keys = function(f, initial, a) {
  return Object.keys(a).reduce(function(acc, key) {
    return f(acc, key, a[key])
  }, initial)
};

var merge = function(a, b) {
  var obj = {}
  for (var key in b) obj[key] = b[key]
  for (var key in a) obj[key] = a[key]
  return obj
};
|};

[@bs.val]
external fold_left : (('a, 'b) => 'a, 'a, Js.Dict.t('b)) => 'a = "fold_left";
[@bs.val]
external fold_left_keys : (('a, string, 'b) => 'a, 'a, Js.Dict.t('b)) => 'a =
  "fold_left_keys";
[@bs.val]
external merge : (Js.Dict.t('a), Js.Dict.t('a)) => Js.Dict.t('a) = "merge";

external unsafe_from_object : Js.t('a) => Js.Dict.t('b) = "%identity";

let insert: (string, 'a, Js.Dict.t('a)) => Js.Dict.t('a) =
  (key, value, dict) => {
    Js.Dict.set(dict, key, value);
    dict;
  };

module type TRAVERSABLE_F =
  (A: APPLICATIVE) =>
  TRAVERSABLE with
    type t('a) = Js.Dict.t('a) and type applicative_t('a) = A.t('a);

module Functor: FUNCTOR with type t('a) = Js.Dict.t('a) = {
  type t('a) = Js.Dict.t('a);
  let map = (f, a) => Js.Dict.map((. x) => f(x), a);
};

module Apply: APPLY with type t('a) = Js.Dict.t('a) = {
  include Functor;
  let apply = (fn_array, a) =>
    fold_left(
      (acc, f) => merge(acc, map(f, a)),
      Obj.magic(Js.Dict.empty()),
      fn_array,
    );
};

module Alt: ALT with type t('a) = Js.Dict.t('a) = {
  include Functor;
  let alt = merge;
};

module Plus: PLUS with type t('a) = Js.Dict.t('a) = {
  include Alt;
  let empty = Obj.magic(Js.Dict.empty());
};

module Foldable: FOLDABLE with type t('a) = Js.Dict.t('a) = {
  type t('a) = Js.Dict.t('a);

  let fold_left = fold_left
  and fold_right = (f, init, a) =>
    ArrayLabels.fold_right(~f, ~init, Js.Dict.values(a));

  module Fold_Map = (M: MONOID) => {
    module D =
      Default.Fold_Map(
        M,
        {
          type t('a) = Js.Dict.t('a);
          let (fold_left, fold_right) = (fold_left, fold_right);
        },
      );
    let fold_map = D.fold_map_default_left;
  };
  module Fold_Map_Any = (M: MONOID_ANY) => {
    module D =
      Default.Fold_Map_Any(
        M,
        {
          type t('a) = Js.Dict.t('a);
          let (fold_left, fold_right) = (fold_left, fold_right);
        },
      );
    let fold_map = D.fold_map_default_left;
  };
  module Fold_Map_Plus = (P: PLUS) => {
    module D =
      Default.Fold_Map_Plus(
        P,
        {
          type t('a) = Js.Dict.t('a);
          let (fold_left, fold_right) = (fold_left, fold_right);
        },
      );
    let fold_map = D.fold_map_default_left;
  };
};

module Traversable: TRAVERSABLE_F =
  (A: APPLICATIVE) => {
    type t('a) = Js.Dict.t('a)
    and applicative_t('a) = A.t('a);

    include (Functor: FUNCTOR with type t('a) := t('a));
    include (Foldable: FOLDABLE with type t('a) := t('a));

    module I = Infix.Apply(A);

    let traverse_with_index = (f, a) =>
      I.(
        fold_left_keys(
          (acc, k, v) => Function.flip(insert(k)) <$> acc <*> f(k, v),
          A.pure(Js.Dict.empty()),
          a,
        )
      );

    let traverse = Obj.magic(traverse_with_index <. Function.const);

    module D =
      Default.Sequence({
        type t('a) = Js.Dict.t('a)
        and applicative_t('a) = A.t('a);
        let traverse = traverse;
      });

    let sequence = D.sequence_default;
  };
