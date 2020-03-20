/**
 These helpers provide generative tests for implementations.
 */
module type TEST = {
  type test;
  type suite('a);
  type check('a);

  let int: check(int);
  let string: check(string);
  let array: check('a) => check(array('a));
  let option: check('a) => check(option('a));
  let list: check('a) => check(list('a));
  let tuple: (check('a), check('b)) => check(('a, 'b));

  let check: (check('a), ~name: string=?, 'a, 'a) => unit;
  let test: (string, unit => unit) => test;
  let suite: (string, list(test)) => suite(test);
};

module type ARBITRARY = {
  type t;
  type arbitrary('a);
  let make: arbitrary(t);
};

module type ARBITRARY_A = {
  /** Generic helper to make an [arbitrary(t('a))] type from a given [arbitrary('a)] type. */
  type t('a);
  type arbitrary('a);
  let make: arbitrary('a) => arbitrary(t('a));
  let make_bound: arbitrary('a) => arbitrary(t('a));
};

module type QUICKCHECK = {
  /**
  This module type is required to create the generative tests. Provide the framework-specific
  implementations here (ie: qcheck, jsverify,etc).
  */
  type t;
  type arbitrary('a);

  let arbitrary_int: arbitrary(int);

  let property:
    (~count: int=?, ~name: string=?, arbitrary('a), 'a => bool) => t;

  let property2:
    (
      ~count: int=?,
      ~name: string=?,
      arbitrary('a),
      arbitrary('b),
      ('a, 'b) => bool
    ) =>
    t;

  let property3:
    (
      ~count: int=?,
      ~name: string=?,
      arbitrary('a),
      arbitrary('b),
      arbitrary('c),
      ('a, 'b, 'c) => bool
    ) =>
    t;

  let property4:
    (
      ~count: int=?,
      ~name: string=?,
      arbitrary('a),
      arbitrary('b),
      arbitrary('c),
      arbitrary('d),
      ('a, 'b, 'c, 'd) => bool
    ) =>
    t;
};

module Make = (T: TEST, Q: QUICKCHECK with type t = T.test) => {
  type t;
  type arbitrary('a);

  let (<.) = Function.Infix.(<.);

  module Functor =
         (
           F: Interface.FUNCTOR,
           FA:
             ARBITRARY_A with
               type t('a) := F.t('a) and
               type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Functor(F);

    let suite = name => {
      T.suite(
        name ++ ".Functor",
        [
          Q.property(
            ~name="should satisfy identity",
            FA.make(Q.arbitrary_int),
            V.identity,
          ),
          Q.property(
            ~name="should satisfy composition", FA.make(Q.arbitrary_int), a =>
            V.composition((++)("!"), string_of_int, a)
          ),
        ],
      );
    };
  };

  module Apply =
         (
           A: Interface.APPLICATIVE,
           FA:
             ARBITRARY_A with
               type t('a) := A.t('a) and
               type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Apply(A);

    let suite = name => {
      T.suite(
        name ++ ".Apply",
        [
          Q.property(
            ~name="should satisfy associative composition",
            FA.make(Q.arbitrary_int),
            n =>
            V.associative_composition(
              A.pure((++)("!")),
              A.pure(string_of_int),
              n,
            )
          ),
        ],
      );
    };
  };

  module Applicative =
         (
           A: Interface.APPLICATIVE,
           FA:
             ARBITRARY_A with
               type t('a) := A.t('a) and
               type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Applicative(A);

    let suite = name => {
      T.suite(
        name ++ ".Applicative",
        [
          Q.property(
            ~name="should satisfy identity",
            FA.make(Q.arbitrary_int),
            V.identity,
          ),
          Q.property(
            ~name="should satisfy homomorphism",
            FA.make(Q.arbitrary_int),
            V.homomorphism(A.map(string_of_int)),
          ),
          Q.property(
            ~name="should satisfy interchange",
            Q.arbitrary_int,
            V.interchange(A.pure(string_of_int)),
          ),
        ],
      );
    };
  };

  module Monad =
         (
           M: Interface.MONAD,
           FA:
             ARBITRARY_A with
               type t('a) := M.t('a) and
               type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Monad(M);

    let suite = name => {
      T.suite(
        name ++ ".Monad",
        [
          Q.property(
            ~name="should satisfy associativity",
            FA.make_bound(Q.arbitrary_int),
            V.associativity(M.pure <. string_of_int, M.pure <. (++)("!"))
          ),
          Q.property(
            ~name="should satisfy identity",
            Q.arbitrary_int,
            V.identity(M.pure <. string_of_int),
          ),
        ],
      );
    };
  };

  module Alt =
         (
           A: Interface.ALT,
           FA:
             ARBITRARY_A with
               type t('a) := A.t('a) and
               type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Alt(A);

    let suite = name => {
      T.suite(
        name ++ ".Alt",
        [
          Q.property3(
            ~name="should satisfy associativity",
            FA.make(Q.arbitrary_int),
            FA.make(Q.arbitrary_int),
            FA.make(Q.arbitrary_int),
            V.associativity
          ),
          Q.property2(
            ~name="should satisfy distributivity",
            FA.make(Q.arbitrary_int),
            FA.make(Q.arbitrary_int),
            V.distributivity(string_of_int),
          ),
        ],
      );
    };
  };

  module Eq =
         (
           E: Interface.EQ,
           A:
             ARBITRARY with
               type t := E.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Eq(E);

    let suite = name => {
      T.suite(
        name ++ ".Eq",
        [
          Q.property(
            ~name="should satisfy reflexivity",
            A.make,
            V.reflexivity,
          ),
          Q.property2(
            ~name="should satisfy symmetry",
            A.make,
            A.make,
            V.symmetry,
          ),
          Q.property3(
            ~name="should satisfy transitivity",
            A.make,
            A.make,
            A.make,
            V.transitivity,
          ),
        ],
      );
    };
  };

  module Ord =
         (
           O: Interface.ORD,
           A:
             ARBITRARY with
               type t := O.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Ord(O);

    let suite = name => {
      T.suite(
        name ++ ".Ord",
        [
          Q.property(
            ~name="should satisfy reflexivity",
            A.make,
            V.reflexivity,
          ),
          Q.property2(
            ~name="should satisfy antisymmetry",
            A.make,
            A.make,
            V.antisymmetry,
          ),
          Q.property3(
            ~name="should satisfy transitivity",
            A.make,
            A.make,
            A.make,
            V.transitivity,
          ),
        ],
      );
    };
  };

  module Invariant =
         (
           I: Interface.INVARIANT,
           A:
             ARBITRARY_A with
               type t('a) := I.t('a) and
               type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Invariant(I);

    let suite = name => {
      T.suite(
        name ++ ".Invariant",
        [
          Q.property(
            ~name="should satisfy reflexivity",
            A.make(Q.arbitrary_int),
            V.identity,
          ),
          Q.property(
            ~name="should satisfy composition",
            A.make(Q.arbitrary_int),
            V.composition(
              float_of_int,
              int_of_float,
              ( * )(3) <. int_of_float,
              ( *. )(4.0) <. float_of_int,
            ),
          ),
        ],
      );
    };
  };
};

module Array =
       (
         T: TEST,
         Q: QUICKCHECK with type t = T.test,
         A:
           ARBITRARY_A with
             type t('a) = array('a) and
             type arbitrary('a) = Q.arbitrary('a),
         AI:
           ARBITRARY with
             type t = array(int) and type arbitrary('a) = Q.arbitrary('a),
       ) => {
  module M = Make(T, Q);

  module Functor = M.Functor(Array.Functor, A);
  module Apply = M.Apply(Array.Applicative, A);
  module Applicative = M.Applicative(Array.Applicative, A);
  module Monad = M.Monad(Array.Monad, A);
  module Alt = M.Alt(Array.Alt, A);
  module Eq = M.Eq(Functors.ArrayF.Int.Eq, AI);
  module Ord = M.Ord(Functors.ArrayF.Int.Ord, AI);
  module Invariant = M.Invariant(Array.Invariant, A);

  let zip_with =
    T.suite(
      "Array.zip_with",
      [
        T.test("should zip_with two arrays", () => {
          T.check(
            T.array(T.int),
            Array.zip_with(( * ), [|1, 2, 3|], [|4, 5, 6|]),
            [|4, 10, 18|],
          )
        }),
      ],
    );

  let zip =
    T.suite(
      "Array.zip",
      [
        T.test("should zip two arrays", () => {
          T.check(
            T.array(T.tuple(T.int, T.string)),
            Array.zip([|1, 2, 3|], [|"a", "b", "c"|]),
            [|(1, "a"), (2, "b"), (3, "c")|],
          )
        }),
      ],
    );

  let foldable =
    T.suite(
      "Array.Foldable",
      [
        T.test("should do a left fold", () => {
          T.check(
            T.int,
            Array.Foldable.fold_left((+), 0, [|1, 2, 3, 4, 5|]),
            15,
          );
          T.check(T.int, Array.Foldable.fold_left((-), 10, [|3, 2, 1|]), 4);
        }),
        T.test("should do a right fold", () => {
          T.check(
            T.int,
            Array.Foldable.fold_right((-), 10, [|3, 2, 1|]),
            -8,
          )
        }),
        T.test("should do a map fold (int)", () => {
          let fold_map = Functors.ArrayF.Int.Additive.Fold_Map.fold_map;
          T.check(T.int, fold_map(Function.Category.id, [|1, 2, 3|]), 6);
        }),
        T.test("should do a map fold (list)", () => {
          let fold_map = Functors.ArrayF.List.Fold_Map_Plus.fold_map;
          T.check(
            T.list(T.list(T.int)),
            fold_map(List.Applicative.pure, [|[1, 2, 3], [4, 5]|]),
            [[1, 2, 3], [4, 5]],
          );
        }),
      ],
    );

  let unfoldable =
    T.suite(
      "Array.Unfoldable",
      [
        T.test("should do an unfold", () => {
          T.check(
            T.array(T.int),
            Array.Unfoldable.unfold(
              x =>
                if (x > 5) {
                  None;
                } else {
                  Some((x, x + 1));
                },
              0,
            ),
            [|0, 1, 2, 3, 4, 5|],
          )
        }),
        T.test("should do an unfold", () => {
          T.check(
            T.array(T.int),
            Array.Unfoldable.unfold(
              x =>
                if (x > 20) {
                  None;
                } else {
                  Some((x, x + 5));
                },
              0,
            ),
            [|0, 5, 10, 15, 20|],
          )
        }),
      ],
    );

  let traversable = {
    let (traverse, sequence) =
      Functors.ArrayF.Option.Traversable.(traverse, sequence);

    T.suite(
      "Array.Traversable",
      [
        T.test("should traverse the array", () => {
          let positive_int = x => x >= 0 ? Some(x) : None;
          T.check(
            T.option(T.array(T.int)),
            traverse(positive_int, [|1, 2, 3|]),
            Some([|1, 2, 3|]),
          );
          T.check(
            T.option(T.array(T.int)),
            traverse(positive_int, [|1, 2, (-3)|]),
            None,
          );
        }),
        T.test("should sequence the array", () => {
          T.check(
            T.option(T.array(T.int)),
            sequence([|Some(3), Some(4), Some(5)|]),
            Some([|3, 4, 5|]),
          );
          T.check(
            T.option(T.array(T.int)),
            sequence([|Some(3), Some(4), None|]),
            None,
          );
        }),
      ],
    );
  };

  let show = {
    module S = Array.Show(Int.Show);

    T.suite(
      "Array.Show",
      [
        T.test("should show the array", () => {
          T.check(
            T.string,
            S.show([|1, 1, 2, 3, 5, 8, 13|]),
            "[1, 1, 2, 3, 5, 8, 13]",
          )
        }),
      ],
    );
  };

  let extend = {
    module V = Verify.Extend(Array.Extend);
    let id = Function.Category.id;
    let (<.) = Function.Infix.(<.);
    let fold = Functors.ArrayF.Int.Additive.Fold_Map.fold_map(id);
    let fold' = Functors.ArrayF.Float.Additive.Fold_Map.fold_map(id);

    T.suite(
      "Array.Extend",
      [
        Q.property(
          ~name="should satisfy associativity",
          A.make_bound(Q.arbitrary_int),
          V.associativity(string_of_float <. fold', float_of_int <. fold),
        ),
      ],
    );
  };

  let suites =
    [
      Functor.suite,
      Apply.suite,
      Applicative.suite,
      Monad.suite,
      Alt.suite,
      Eq.suite,
      Ord.suite,
      Invariant.suite,
    ]
    |> ListLabels.map(~f=suite => suite("Array"))
    |> ListLabels.append([
         zip_with,
         zip,
         foldable,
         unfoldable,
         traversable,
         show,
         extend,
       ]);
};
