/**
 These helpers provide generative tests for implementations.
 */
module type TEST = {
  type test;
  type suite('a);
  type check('a);

  let int: check(int);
  let bool: check(bool);
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

  module Compare = {
    module Medial_Magma =
           (
             M: Interface.MEDIAL_MAGMA,
             E: Interface.EQ with type t = M.t,
             A:
               ARBITRARY with
                 type t := M.t and type arbitrary('a) := Q.arbitrary('a),
           ) => {
      module V = Verify.Compare.Medial_Magma(M, E);

      let suite = name => {
        T.suite(
          name ++ ".Medial_Magma",
          [
            Q.property4(
              ~name="should satisfy bicommutativity",
              A.make,
              A.make,
              A.make,
              A.make,
              V.bicommutativity,
            ),
          ],
        );
      };
    };

    module Quasigroup =
           (
             QG: Interface.QUASIGROUP,
             E: Interface.EQ with type t = QG.t,
             A:
               ARBITRARY with
                 type t := QG.t and type arbitrary('a) := Q.arbitrary('a),
           ) => {
      module V = Verify.Compare.Quasigroup(QG, E);

      let suite = name => {
        T.suite(
          name ++ ".Quasigroup",
          [
            Q.property3(
              ~name="should satisfy associativity",
              A.make,
              A.make,
              A.make,
              V.cancellative,
            ),
          ],
        );
      };
    };

    module Semiring =
           (
             S: Interface.SEMIRING,
             E: Interface.EQ with type t = S.t,
             A:
               ARBITRARY with
                 type t := S.t and type arbitrary('a) := Q.arbitrary('a),
           ) => {
      module V = Verify.Compare.Semiring(S, E);

      let suite = name => {
        T.suite(
          name ++ ".Semiring",
          [
            Q.property3(
              ~name="should satisfy additive associativity",
              A.make,
              A.make,
              A.make,
              V.additive_associativity,
            ),
            Q.property(
              ~name="should satisfy additive identity",
              A.make,
              V.additive_identity,
            ),
            Q.property2(
              ~name="should satisfy commutativity",
              A.make,
              A.make,
              V.commutativity,
            ),
            Q.property3(
              ~name="should satisfy multiplicative associativity",
              A.make,
              A.make,
              A.make,
              V.multiplicative_associativity,
            ),
            Q.property(
              ~name="should satisfy multiplicative identity",
              A.make,
              V.multiplicative_identity,
            ),
            Q.property3(
              ~name="should satisfy distributivity",
              A.make,
              A.make,
              A.make,
              V.distributivity,
            ),
          ],
        );
      };
    };

    module Division_Ring =
           (
             D: Interface.DIVISION_RING,
             E: Interface.EQ with type t = D.t,
             A:
               ARBITRARY with
                 type t := D.t and type arbitrary('a) := Q.arbitrary('a),
           ) => {
      module V = Verify.Compare.Division_Ring(D, E);

      let suite = name => {
        T.suite(
          name ++ ".Division_Ring",
          [
            T.test("should be a non-zero ring (zero is not one)", () => {
              T.check(T.bool, V.non_zero_ring, true)
            }),
            Q.property(
              ~name="should satisfy multiplicative inverse",
              A.make,
              V.multiplicative_inverse,
            ),
          ],
        );
      };
    };

    module Euclidean_Ring =
           (
             E: Interface.EUCLIDEAN_RING,
             EQ: Interface.EQ with type t = E.t,
             A:
               ARBITRARY with
                 type t := E.t and type arbitrary('a) := Q.arbitrary('a),
           ) => {
      module V = Verify.Compare.Euclidean_Ring(E, EQ);

      let suite = name => {
        T.suite(
          name ++ ".Euclidean_Ring",
          [
            T.test("should be a non-zero ring (zero is not one)", () => {
              T.check(T.bool, V.non_zero_ring, true)
            }),
            Q.property2(
              ~name="should satisfy integral domain",
              A.make,
              A.make,
              V.integral_domain,
            ),
            Q.property(
              ~name="should satisfy non negative degree",
              A.make,
              V.non_negative_degree,
            ),
            Q.property2(
              ~name="should satisfy the properties for remainder",
              A.make,
              A.make,
              V.remainder,
            ),
            Q.property2(
              ~name="should satisfy submultiplicative",
              A.make,
              A.make,
              V.submultiplicative,
            ),
          ],
        );
      };
    };
  };

  module Medial_Magma =
         (
           M: Interface.MEDIAL_MAGMA,
           A:
             ARBITRARY with
               type t := M.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Medial_Magma(M);

    let suite = name => {
      T.suite(
        name ++ ".Medial_Magma",
        [
          Q.property4(
            ~name="should satisfy bicommutativity",
            A.make,
            A.make,
            A.make,
            A.make,
            V.bicommutativity,
          ),
        ],
      );
    };
  };

  module Semigroup =
         (
           S: Interface.SEMIGROUP,
           A:
             ARBITRARY with
               type t := S.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Semigroup(S);

    let suite = name => {
      T.suite(
        name ++ ".Semigroup",
        [
          Q.property3(
            ~name="should satisfy associativity",
            A.make,
            A.make,
            A.make,
            V.associativity,
          ),
        ],
      );
    };
  };

  module Quasigroup =
         (
           QG: Interface.QUASIGROUP,
           A:
             ARBITRARY with
               type t := QG.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Quasigroup(QG);

    let suite = name => {
      T.suite(
        name ++ ".Quasigroup",
        [
          Q.property3(
            ~name="should satisfy associativity",
            A.make,
            A.make,
            A.make,
            V.cancellative,
          ),
        ],
      );
    };
  };

  module Loop =
         (
           L: Interface.LOOP,
           A:
             ARBITRARY with
               type t := L.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Loop(L);

    let suite = name => {
      T.suite(
        name ++ ".Loop",
        [Q.property(~name="should satisfy identity", A.make, V.identity)],
      );
    };
  };

  module Group =
         (
           G: Interface.GROUP,
           A:
             ARBITRARY with
               type t := G.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Group(G);

    let suite = name => {
      T.suite(
        name ++ ".Group",
        [
          Q.property(
            ~name="should satisfy invertibility",
            A.make,
            V.invertibility,
          ),
          Q.property3(
            ~name="should satisfy associativity",
            A.make,
            A.make,
            A.make,
            V.associativity,
          ),
        ],
      );
    };
  };

  module Abelian_Group =
         (
           G: Interface.ABELIAN_GROUP,
           A:
             ARBITRARY with
               type t := G.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Abelian_Group(G);

    let suite = name => {
      T.suite(
        name ++ ".Abelian_Group",
        [
          Q.property2(
            ~name="should satisfy commutativity",
            A.make,
            A.make,
            V.commutativity,
          ),
        ],
      );
    };
  };

  module Monoid =
         (
           M: Interface.MONOID,
           A:
             ARBITRARY with
               type t := M.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Monoid(M);

    let suite = name => {
      T.suite(
        name ++ ".Monoid",
        [Q.property(~name="should satisfy identity", A.make, V.identity)],
      );
    };
  };

  module Functor =
         (
           F: Interface.FUNCTOR,
           AA:
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
            AA.make(Q.arbitrary_int),
            V.identity,
          ),
          Q.property(
            ~name="should satisfy composition", AA.make(Q.arbitrary_int), a =>
            V.composition((++)("!"), string_of_int, a)
          ),
        ],
      );
    };
  };

  module Apply =
         (
           A: Interface.APPLICATIVE,
           AA:
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
            AA.make(Q.arbitrary_int),
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
           AA:
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
            AA.make(Q.arbitrary_int),
            V.identity,
          ),
          Q.property(
            ~name="should satisfy homomorphism",
            AA.make(Q.arbitrary_int),
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
           AA:
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
            AA.make_bound(Q.arbitrary_int),
            V.associativity(M.pure <. string_of_int, M.pure <. (++)("!")),
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
           AA:
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
            AA.make(Q.arbitrary_int),
            AA.make(Q.arbitrary_int),
            AA.make(Q.arbitrary_int),
            V.associativity,
          ),
          Q.property2(
            ~name="should satisfy distributivity",
            AA.make(Q.arbitrary_int),
            AA.make(Q.arbitrary_int),
            V.distributivity(string_of_int),
          ),
        ],
      );
    };
  };

  module Alternative =
         (
           A: Interface.ALTERNATIVE,
           AA:
             ARBITRARY_A with
               type t('a) := A.t('a) and
               type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Alternative(A);

    let suite = name => {
      T.suite(
        name ++ ".Alternative",
        [
          Q.property(
            ~name="should satisfy distributivity",
            AA.make(Q.arbitrary_int),
            V.distributivity(A.pure(( * )(2)), A.pure((+)(3))),
          ),
          T.test("should satisfy annihalation", () => {
            T.check(T.bool, V.annihalation(string_of_int |> A.pure), true)
          }),
        ],
      );
    };
  };

  module Plus =
         (
           P: Interface.PLUS,
           AA:
             ARBITRARY_A with
               type t('a) := P.t('a) and
               type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Plus(P);

    let suite = name => {
      T.suite(
        name ++ ".Plus",
        [
          Q.property(
            ~name="should satisfy identity",
            AA.make(Q.arbitrary_int),
            V.identity,
          ),
          T.test("should satisfy annihalation", () => {
            T.check(T.bool, V.annihalation(string_of_int), true)
          }),
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

  module Join_Semilattice =
         (
           JS: Interface.JOIN_SEMILATTICE,
           A:
             ARBITRARY with
               type t := JS.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Join_Semilattice(JS);

    let suite = name => {
      T.suite(
        name ++ ".Join_Semilattice",
        [
          Q.property3(
            ~name="should satisfy associativity",
            A.make,
            A.make,
            A.make,
            V.associativity,
          ),
          Q.property2(
            ~name="should satisfy commutativity",
            A.make,
            A.make,
            V.commutativity,
          ),
          Q.property(
            ~name="should satisfy idempotency",
            A.make,
            V.idempotency,
          ),
        ],
      );
    };
  };

  module Meet_Semilattice =
         (
           MS: Interface.MEET_SEMILATTICE,
           A:
             ARBITRARY with
               type t := MS.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Meet_Semilattice(MS);

    let suite = name => {
      T.suite(
        name ++ ".Meet_Semilattice",
        [
          Q.property3(
            ~name="should satisfy associativity",
            A.make,
            A.make,
            A.make,
            V.associativity,
          ),
          Q.property2(
            ~name="should satisfy commutativity",
            A.make,
            A.make,
            V.commutativity,
          ),
          Q.property(
            ~name="should satisfy idempotency",
            A.make,
            V.idempotency,
          ),
        ],
      );
    };
  };

  module Bounded_Join_Semilattice =
         (
           BJS: Interface.BOUNDED_JOIN_SEMILATTICE,
           A:
             ARBITRARY with
               type t := BJS.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Bounded_Join_Semilattice(BJS);

    let suite = name => {
      T.suite(
        name ++ ".Bounded_Join_Semilattice",
        [Q.property(~name="should satisfy identity", A.make, V.identity)],
      );
    };
  };

  module Bounded_Meet_Semilattice =
         (
           BMS: Interface.BOUNDED_MEET_SEMILATTICE,
           A:
             ARBITRARY with
               type t := BMS.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Bounded_Meet_Semilattice(BMS);

    let suite = name => {
      T.suite(
        name ++ ".Bounded_Meet_Semilattice",
        [Q.property(~name="should satisfy identity", A.make, V.identity)],
      );
    };
  };

  module Lattice =
         (
           L: Interface.LATTICE,
           A:
             ARBITRARY with
               type t := L.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Lattice(L);

    let suite = name => {
      T.suite(
        name ++ ".Lattice",
        [
          Q.property2(
            ~name="should satisfy absorption",
            A.make,
            A.make,
            V.absorption,
          ),
        ],
      );
    };
  };

  module Bounded_Lattice =
         (
           BL: Interface.BOUNDED_LATTICE,
           A:
             ARBITRARY with
               type t := BL.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Bounded_Lattice(BL);

    let suite = name => {
      T.suite(
        name ++ ".Bounded_Lattice",
        [
          Q.property2(
            ~name="should satisfy absorption",
            A.make,
            A.make,
            V.absorption,
          ),
        ],
      );
    };
  };

  module Distributive_Lattice =
         (
           DL: Interface.DISTRIBUTIVE_LATTICE,
           A:
             ARBITRARY with
               type t := DL.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Distributive_Lattice(DL);

    let suite = name => {
      T.suite(
        name ++ ".Distributive_Lattice",
        [
          Q.property3(
            ~name="should satisfy distributivity",
            A.make,
            A.make,
            A.make,
            V.distributivity,
          ),
        ],
      );
    };
  };

  module Bounded_Distributive_Lattice =
         (
           BDL: Interface.BOUNDED_DISTRIBUTIVE_LATTICE,
           A:
             ARBITRARY with
               type t := BDL.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Bounded_Distributive_Lattice(BDL);

    let suite = name => {
      T.suite(
        name ++ ".Bounded_Distributive_Lattice",
        [
          Q.property3(
            ~name="should satisfy distributivity",
            A.make,
            A.make,
            A.make,
            V.distributivity,
          ),
        ],
      );
    };
  };

  module Heyting_Algebra =
         (
           HA: Interface.HEYTING_ALGEBRA,
           A:
             ARBITRARY with
               type t := HA.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Heyting_Algebra(HA);

    let suite = name => {
      T.suite(
        name ++ ".Heyting_Algebra",
        [
          Q.property(
            ~name="should satisfy pseudocomplement",
            A.make,
            V.pseudocomplement,
          ),
          Q.property3(
            ~name="should satisfy relative pseudocomplement",
            A.make,
            A.make,
            A.make,
            V.relative_pseudocomplement,
          ),
        ],
      );
    };
  };

  module Involutive_Heyting_Algebra =
         (
           IHA: Interface.INVOLUTIVE_HEYTING_ALGEBRA,
           A:
             ARBITRARY with
               type t := IHA.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Involutive_Heyting_Algebra(IHA);

    let suite = name => {
      T.suite(
        name ++ ".Involutive_Heyting_Algebra",
        [
          Q.property(~name="should satisfy involution", A.make, V.involution),
        ],
      );
    };
  };

  module Boolean_Algebra =
         (
           BA: Interface.BOOLEAN_ALGEBRA,
           A:
             ARBITRARY with
               type t := BA.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Boolean_Algebra(BA);

    let suite = name => {
      T.suite(
        name ++ ".Boolean_Algebra",
        [
          Q.property(
            ~name="should satisfy the law of excluded middle",
            A.make,
            V.excluded_middle,
          ),
        ],
      );
    };
  };

  module Bounded =
         (
           B: Interface.BOUNDED,
           A:
             ARBITRARY with
               type t := B.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Bounded(B);

    let suite = name => {
      T.suite(
        name ++ ".Bounded",
        [Q.property(~name="should satisfy bounded", A.make, V.bounded)],
      );
    };
  };

  module Semiring =
         (
           S: Interface.SEMIRING,
           A:
             ARBITRARY with
               type t := S.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Semiring(S);

    let suite = name => {
      T.suite(
        name ++ ".Semiring",
        [
          Q.property3(
            ~name="should satisfy additive associativity",
            A.make,
            A.make,
            A.make,
            V.additive_associativity,
          ),
          Q.property(
            ~name="should satisfy additive identity",
            A.make,
            V.additive_identity,
          ),
          Q.property2(
            ~name="should satisfy commutativity",
            A.make,
            A.make,
            V.commutativity,
          ),
          Q.property3(
            ~name="should satisfy multiplicative associativity",
            A.make,
            A.make,
            A.make,
            V.multiplicative_associativity,
          ),
          Q.property(
            ~name="should satisfy multiplicative identity",
            A.make,
            V.multiplicative_identity,
          ),
          Q.property3(
            ~name="should satisfy distributivity",
            A.make,
            A.make,
            A.make,
            V.distributivity,
          ),
        ],
      );
    };
  };

  module Ring =
         (
           R: Interface.RING,
           A:
             ARBITRARY with
               type t := R.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Ring(R);

    let suite = name => {
      T.suite(
        name ++ ".Ring",
        [
          Q.property(
            ~name="should satisfy additive inverse",
            A.make,
            V.additive_inverse,
          ),
        ],
      );
    };
  };

  module Commutative_Ring =
         (
           C: Interface.COMMUTATIVE_RING,
           A:
             ARBITRARY with
               type t := C.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Commutative_Ring(C);

    let suite = name => {
      T.suite(
        name ++ ".Commutative_Ring",
        [
          Q.property2(
            ~name="should satisfy multiplicative commutativity",
            A.make,
            A.make,
            V.multiplicative_commutativity,
          ),
        ],
      );
    };
  };

  module Division_Ring =
         (
           D: Interface.DIVISION_RING,
           A:
             ARBITRARY with
               type t := D.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Division_Ring(D);

    let suite = name => {
      T.suite(
        name ++ ".Division_Ring",
        [
          T.test("should be a non-zero ring (zero is not one)", () => {
            T.check(T.bool, V.non_zero_ring, true)
          }),
          Q.property(
            ~name="should satisfy multiplicative inverse",
            A.make,
            V.multiplicative_inverse,
          ),
        ],
      );
    };
  };

  module Euclidean_Ring =
         (
           E: Interface.EUCLIDEAN_RING,
           A:
             ARBITRARY with
               type t := E.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Euclidean_Ring(E);

    let suite = name => {
      T.suite(
        name ++ ".Euclidean_Ring",
        [
          T.test("should be a non-zero ring (zero is not one)", () => {
            T.check(T.bool, V.non_zero_ring, true)
          }),
          Q.property2(
            ~name="should satisfy integral domain",
            A.make,
            A.make,
            V.integral_domain,
          ),
          Q.property(
            ~name="should satisfy non negative degree",
            A.make,
            V.non_negative_degree,
          ),
          Q.property2(
            ~name="should satisfy the properties for remainder",
            A.make,
            A.make,
            V.remainder,
          ),
          Q.property2(
            ~name="should satisfy submultiplicative",
            A.make,
            A.make,
            V.submultiplicative,
          ),
        ],
      );
    };
  };

  module Field =
         (
           F: Interface.FIELD,
           A:
             ARBITRARY with
               type t := F.t and type arbitrary('a) := Q.arbitrary('a),
         ) => {
    module V = Verify.Field(F);

    let suite = name => {
      T.suite(
        name ++ ".Field",
        [
          Q.property2(
            ~name="should satisfy non zero multiplicative inverse",
            A.make,
            A.make,
            V.non_zero_multiplicative_inverse,
          ),
        ],
      );
    };
  };

  module Invariant =
         (
           I: Interface.INVARIANT,
           AA:
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
            AA.make(Q.arbitrary_int),
            V.identity,
          ),
          Q.property(
            ~name="should satisfy composition",
            AA.make(Q.arbitrary_int),
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
         Arr: ArrayF.ARRAY,
         T: TEST,
         Q: QUICKCHECK with type t = T.test,
         A:
           ARBITRARY with
             type t = array(int) and type arbitrary('a) = Q.arbitrary('a),
         AA:
           ARBITRARY_A with
             type t('a) = array('a) and
             type arbitrary('a) = Q.arbitrary('a),
       ) => {
  module M = Make(T, Q);

  module Functor = M.Functor(Arr.Functor, AA);
  module Apply = M.Apply(Arr.Applicative, AA);
  module Applicative = M.Applicative(Arr.Applicative, AA);
  module Monad = M.Monad(Arr.Monad, AA);
  module Alt = M.Alt(Arr.Alt, AA);
  module Eq = M.Eq(Functors.ArrayF.Int.Eq, A);
  module Ord = M.Ord(Functors.ArrayF.Int.Ord, A);
  module Invariant = M.Invariant(Arr.Invariant, AA);

  let zip_with =
    T.suite(
      "Array.zip_with",
      [
        T.test("should zip_with two arrays", () => {
          T.check(
            T.array(T.int),
            Arr.zip_with(( * ), [|1, 2, 3|], [|4, 5, 6|]),
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
            Arr.zip([|1, 2, 3|], [|"a", "b", "c"|]),
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
            Arr.Foldable.fold_left((+), 0, [|1, 2, 3, 4, 5|]),
            15,
          );
          T.check(T.int, Arr.Foldable.fold_left((-), 10, [|3, 2, 1|]), 4);
        }),
        T.test("should do a right fold", () => {
          T.check(T.int, Arr.Foldable.fold_right((-), 10, [|3, 2, 1|]), -8)
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
            Arr.Unfoldable.unfold(
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
            Arr.Unfoldable.unfold(
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
    module S = Arr.Show(Int.Show);

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
    module V = Verify.Extend(Arr.Extend);
    let id = Function.Category.id;
    let (<.) = Function.Infix.(<.);
    let fold = Functors.ArrayF.Int.Additive.Fold_Map.fold_map(id);
    let fold' = Functors.ArrayF.Float.Additive.Fold_Map.fold_map(id);

    T.suite(
      "Array.Extend",
      [
        Q.property(
          ~name="should satisfy associativity",
          AA.make_bound(Q.arbitrary_int),
          V.associativity(string_of_float <. fold', float_of_int <. fold),
        ),
      ],
    );
  };

  let alt_order = {
    T.suite(
      "Array.Alt.alt",
      [
        T.test("should order the arrays correctly", () => {
          T.check(
            T.array(T.int),
            Arr.Alt.alt([|1, 2, 3|], [|4, 5|]),
            [|1, 2, 3, 4, 5|],
          )
        }),
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
         alt_order,
       ]);
};

module Bool =
       (
         T: TEST,
         Q: QUICKCHECK with type t = T.test,
         A:
           ARBITRARY with
             type t = bool and type arbitrary('a) = Q.arbitrary('a),
       ) => {
  module M = Make(T, Q);

  module Conjunctive = {
    module Medial_Magma = M.Medial_Magma(Bool.Conjunctive.Medial_Magma, A);
    module Semigroup = M.Semigroup(Bool.Conjunctive.Semigroup, A);
    module Monoid = M.Monoid(Bool.Conjunctive.Monoid, A);
  };

  module Disjunctive = {
    module Medial_Magma = M.Medial_Magma(Bool.Disjunctive.Medial_Magma, A);
    module Semigroup = M.Semigroup(Bool.Disjunctive.Semigroup, A);
    module Monoid = M.Monoid(Bool.Disjunctive.Monoid, A);
  };

  module Eq = M.Eq(Bool.Eq, A);
  module Ord = M.Ord(Bool.Ord, A);
  module Join_Semilattice = M.Join_Semilattice(Bool.Join_Semilattice, A);
  module Meet_Semilattice = M.Meet_Semilattice(Bool.Meet_Semilattice, A);
  module Bounded_Join_Semilattice =
    M.Bounded_Join_Semilattice(Bool.Bounded_Join_Semilattice, A);
  module Bounded_Meet_Semilattice =
    M.Bounded_Meet_Semilattice(Bool.Bounded_Meet_Semilattice, A);
  module Lattice = M.Lattice(Bool.Lattice, A);
  module Bounded_Lattice = M.Bounded_Lattice(Bool.Bounded_Lattice, A);
  module Distributive_Lattice =
    M.Distributive_Lattice(Bool.Distributive_Lattice, A);
  module Bounded_Distributive_Lattice =
    M.Bounded_Distributive_Lattice(Bool.Bounded_Distributive_Lattice, A);
  module Heyting_Algebra = M.Heyting_Algebra(Bool.Heyting_Algebra, A);
  module Involutive_Heyting_Algebra =
    M.Involutive_Heyting_Algebra(Bool.Involutive_Heyting_Algebra, A);
  module Boolean_Algebra = M.Boolean_Algebra(Bool.Boolean_Algebra, A);

  let suites =
    ListLabels.concat([
      [
        Conjunctive.Medial_Magma.suite,
        Conjunctive.Semigroup.suite,
        Conjunctive.Monoid.suite,
      ]
      |> ListLabels.map(~f=suite => suite("Bool.Conjunctive")),
      [
        Disjunctive.Medial_Magma.suite,
        Disjunctive.Semigroup.suite,
        Disjunctive.Monoid.suite,
      ]
      |> ListLabels.map(~f=suite => suite("Bool.Disjunctive")),
      [
        Eq.suite,
        Ord.suite,
        Join_Semilattice.suite,
        Meet_Semilattice.suite,
        Bounded_Join_Semilattice.suite,
        Bounded_Meet_Semilattice.suite,
        Lattice.suite,
        Bounded_Lattice.suite,
        Distributive_Lattice.suite,
        Bounded_Distributive_Lattice.suite,
        Heyting_Algebra.suite,
        Involutive_Heyting_Algebra.suite,
        Boolean_Algebra.suite,
      ]
      |> ListLabels.map(~f=suite => suite("Bool")),
    ]);
};

module Default = (T: TEST, Q: QUICKCHECK with type t = T.test) => {
  module Foldable: Interface.FOLDABLE with type t('a) = list('a) = {
    type t('a) = list('a);

    module FM: Default.FOLD_MAP with type t('a) = list('a) = {
      type t('a) = list('a);
      module Fold_Map_Any = (M: Interface.MONOID_ANY) => {
        let fold_map = (f, x) =>
          ListLabels.fold_left(
            ~f=(acc, x) => M.append(acc, f(x)),
            ~init=M.empty,
            x,
          );
      };
      module Fold_Map_Plus = (P: Interface.PLUS) => {
        let fold_map = (f, x) =>
          ListLabels.fold_left(
            ~f=(acc, x) => P.alt(acc, f(x)),
            ~init=P.empty,
            x,
          );
      };
    };
    module Fold_Map = List.Foldable.Fold_Map;
    module Fold_Map_Any = FM.Fold_Map_Any;
    module Fold_Map_Plus = FM.Fold_Map_Plus;
    module F = Default.Fold(FM);

    let (fold_left, fold_right) = (
      F.fold_left_default,
      F.fold_right_default,
    );
  };

  module Traversable = (A: Interface.APPLICATIVE) => {
    module List_Traversable:
      Interface.TRAVERSABLE with
        type applicative_t('a) = A.t('a) and type t('a) = list('a) = {
      type t('a) = list('a);
      type applicative_t('a) = A.t('a);
      include (List.Functor: Interface.FUNCTOR with type t('a) := t('a));
      include (List.Foldable: Interface.FOLDABLE with type t('a) := t('a));

      module I = Infix.Apply(A);
      let sequence = xs =>
        I.(
          ListLabels.fold_right(
            ~f=(acc, x) => A.pure((y, ys) => [y, ...ys]) <*> acc <*> x,
            ~init=A.pure([]),
            xs,
          )
        );

      module D =
        Default.Traverse({
          type t('a) = list('a);
          type applicative_t('a) = A.t('a);
          include (List.Functor: Interface.FUNCTOR with type t('a) := t('a));
          let sequence = sequence;
        });
      let traverse = D.traverse_default;
    };
    include List_Traversable;
  };

  let foldable = {
    Foldable.(
      T.suite(
        "Default.Foldable",
        [
          T.test("should do a left fold", () => {
            T.check(T.int, fold_left((+), 0, [1, 2, 3, 4, 5]), 15);
            T.check(T.int, fold_left((-), 10, [3, 2, 1]), 4);
          }),
        ],
      )
    );
  };

  module Traverse = Traversable(Option.Applicative);

  let traversable = {
    Traverse.(
      T.suite(
        "Default.Traversable",
        [
          T.test("should traverse the list", () => {
            let positive_int = x => x >= 0 ? Some(x) : None;
            T.check(
              T.option(T.list(T.int)),
              traverse(positive_int, [1, 2, 3]),
              Some([1, 2, 3]),
            );
          }),
        ],
      )
    );
  };

  let suites = [foldable, traversable];
};

module Float =
       (
         E: Interface.EQ with type t = float,
         T: TEST,
         Q: QUICKCHECK with type t = T.test,
         A:
           ARBITRARY with
             type t = float and type arbitrary('a) = Q.arbitrary('a),
       ) => {
  module M = Make(T, Q);

  module Additive = {
    module Medial_Magma =
      M.Compare.Medial_Magma(Float.Additive.Medial_Magma, E, A);
    module Semigroup = M.Semigroup(Float.Additive.Semigroup, A);
    module Monoid = M.Monoid(Float.Additive.Monoid, A);
    module Quasigroup = M.Quasigroup(Float.Additive.Quasigroup, A);
    module Loop = M.Loop(Float.Additive.Loop, A);
    module Group = M.Group(Float.Additive.Group, A);
    module Abelian_Group = M.Abelian_Group(Float.Additive.Abelian_Group, A);
  };

  module Multiplicative = {
    module Medial_Magma =
      M.Compare.Medial_Magma(Float.Multiplicative.Medial_Magma, E, A);
    module Semigroup = M.Semigroup(Float.Multiplicative.Semigroup, A);
    module Monoid = M.Monoid(Float.Multiplicative.Monoid, A);
    module Quasigroup =
      M.Compare.Quasigroup(Float.Multiplicative.Quasigroup, E, A);
    module Loop = M.Loop(Float.Multiplicative.Loop, A);
  };

  module Subtractive = {
    module Medial_Magma =
      M.Compare.Medial_Magma(Float.Subtractive.Medial_Magma, E, A);
    module Quasigroup =
      M.Compare.Quasigroup(Float.Subtractive.Quasigroup, E, A);
  };

  module Divisive = {
    module Medial_Magma =
      M.Compare.Medial_Magma(Float.Divisive.Medial_Magma, E, A);
    module Quasigroup = M.Compare.Quasigroup(Float.Divisive.Quasigroup, E, A);
  };

  module Eq = M.Eq(Float.Eq, A);
  module Ord = M.Ord(Float.Ord, A);
  module Bounded = M.Bounded(Float.Bounded, A);
  module Semiring = M.Compare.Semiring(Float.Semiring, E, A);
  module Ring = M.Ring(Float.Ring, A);
  module Commutative_Ring = M.Commutative_Ring(Float.Commutative_Ring, A);
  module Division_Ring = M.Compare.Division_Ring(Float.Division_Ring, E, A);
  module Euclidean_Ring =
    M.Compare.Euclidean_Ring(Float.Euclidean_Ring, E, A);
  module Field = M.Field(Float.Field, A);

  let suites =
    ListLabels.concat([
      [
        Additive.Medial_Magma.suite,
        Additive.Semigroup.suite,
        Additive.Monoid.suite,
        Additive.Quasigroup.suite,
        Additive.Loop.suite,
        Additive.Group.suite,
        Additive.Abelian_Group.suite,
      ]
      |> ListLabels.map(~f=suite => suite("Float.Additive")),
      [
        Multiplicative.Medial_Magma.suite,
        Multiplicative.Semigroup.suite,
        Multiplicative.Monoid.suite,
        Multiplicative.Quasigroup.suite,
        Multiplicative.Loop.suite,
      ]
      |> ListLabels.map(~f=suite => suite("Float.Multiplicative")),
      [Subtractive.Medial_Magma.suite, Subtractive.Quasigroup.suite]
      |> ListLabels.map(~f=suite => suite("Float.Subtractive")),
      [Divisive.Medial_Magma.suite, Divisive.Quasigroup.suite]
      |> ListLabels.map(~f=suite => suite("Float.Divisive")),
      [
        Eq.suite,
        Ord.suite,
        Bounded.suite,
        Semiring.suite,
        Ring.suite,
        Commutative_Ring.suite,
        Division_Ring.suite,
        Euclidean_Ring.suite,
        Field.suite,
      ]
      |> ListLabels.map(~f=suite => suite("Float")),
    ]);
};

module List =
       (
         T: TEST,
         Q: QUICKCHECK with type t = T.test,
         A:
           ARBITRARY with
             type t = list(int) and type arbitrary('a) = Q.arbitrary('a),
         AA:
           ARBITRARY_A with
             type t('a) = list('a) and type arbitrary('a) = Q.arbitrary('a),
       ) => {
  module M = Make(T, Q);

  module Functor = M.Functor(List.Functor, AA);
  module Apply = M.Apply(List.Applicative, AA);
  module Applicative = M.Applicative(List.Applicative, AA);
  module Monad = M.Monad(List.Monad, AA);
  module Alt = M.Alt(List.Alt, AA);
  module Eq = M.Eq(Functors.ListF.Int.Eq, A);

  let foldable =
    T.suite(
      "List.Foldable",
      [
        T.test("should do a left fold", () => {
          T.check(
            T.int,
            List.Foldable.fold_left((+), 0, [1, 2, 3, 4, 5]),
            15,
          );
          T.check(T.int, List.Foldable.fold_left((-), 10, [3, 2, 1]), 4);
        }),
        T.test("should do a right fold", () => {
          T.check(T.int, List.Foldable.fold_right((-), 10, [3, 2, 1]), -8)
        }),
        T.test("should do a map fold (int)", () => {
          let fold_map = Functors.ListF.Int.Additive.Fold_Map.fold_map;
          T.check(T.int, fold_map(Function.Category.id, [1, 2, 3]), 6);
        }),
        T.test("should do a map fold (list)", () => {
          let fold_map = Functors.ListF.List.Fold_Map_Plus.fold_map;
          T.check(
            T.list(T.list(T.int)),
            fold_map(List.Applicative.pure, [[1, 2, 3], [4, 5]]),
            [[1, 2, 3], [4, 5]],
          );
        }),
      ],
    );

  let unfoldable =
    T.suite(
      "List.Unfoldable",
      [
        T.test("should do an unfold", () => {
          T.check(
            T.list(T.int),
            List.Unfoldable.unfold(
              x =>
                if (x > 5) {
                  None;
                } else {
                  Some((x, x + 1));
                },
              0,
            ),
            [0, 1, 2, 3, 4, 5],
          )
        }),
        T.test("should do an unfold", () => {
          T.check(
            T.list(T.int),
            List.Unfoldable.unfold(
              x =>
                if (x > 20) {
                  None;
                } else {
                  Some((x, x + 5));
                },
              0,
            ),
            [0, 5, 10, 15, 20],
          )
        }),
      ],
    );

  let traversable = {
    let (traverse, sequence) =
      Functors.ListF.Option.Traversable.(traverse, sequence);

    T.suite(
      "List.Traversable",
      [
        T.test("should traverse the list", () => {
          let positive_int = x => x >= 0 ? Some(x) : None;
          T.check(
            T.option(T.list(T.int)),
            traverse(positive_int, [1, 2, 3]),
            Some([1, 2, 3]),
          );
          T.check(
            T.option(T.list(T.int)),
            traverse(positive_int, [1, 2, (-3)]),
            None,
          );
        }),
        T.test("should sequence the list", () => {
          T.check(
            T.option(T.list(T.int)),
            sequence([Some(3), Some(4), Some(5)]),
            Some([3, 4, 5]),
          );
          T.check(
            T.option(T.list(T.int)),
            sequence([Some(3), Some(4), None]),
            None,
          );
        }),
      ],
    );
  };

  let show = {
    module S = List.Show(Int.Show);

    T.suite(
      "List.Show",
      [
        T.test("should show the list", () => {
          T.check(
            T.string,
            S.show([1, 1, 2, 3, 5, 8, 13]),
            "[1, 1, 2, 3, 5, 8, 13]",
          )
        }),
      ],
    );
  };

  let alt_order = {
    T.suite(
      "List.Alt.alt",
      [
        T.test("should order the lists correctly", () => {
          T.check(
            T.list(T.int),
            List.Alt.alt([1, 2, 3], [4, 5]),
            [1, 2, 3, 4, 5],
          )
        }),
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
    ]
    |> ListLabels.map(~f=suite => suite("List"))
    |> ListLabels.append([foldable, unfoldable, traversable, show, alt_order]);
};

module Int =
       (
         T: TEST,
         Q: QUICKCHECK with type t = T.test,
         A:
           ARBITRARY with
             type t = int and type arbitrary('a) = Q.arbitrary('a),
       ) => {
  module M = Make(T, Q);

  module Additive = {
    module Medial_Magma = M.Medial_Magma(Int.Additive.Medial_Magma, A);
    module Semigroup = M.Semigroup(Int.Additive.Semigroup, A);
    module Monoid = M.Monoid(Int.Additive.Monoid, A);
    module Quasigroup = M.Quasigroup(Int.Additive.Quasigroup, A);
    module Loop = M.Loop(Int.Additive.Loop, A);
    module Group = M.Group(Int.Additive.Group, A);
    module Abelian_Group = M.Abelian_Group(Int.Additive.Abelian_Group, A);
  };

  module Multiplicative = {
    module Medial_Magma = M.Medial_Magma(Int.Multiplicative.Medial_Magma, A);
    module Semigroup = M.Semigroup(Int.Multiplicative.Semigroup, A);
    module Monoid = M.Monoid(Int.Multiplicative.Monoid, A);
    module Quasigroup = M.Quasigroup(Int.Multiplicative.Quasigroup, A);
    module Loop = M.Loop(Int.Multiplicative.Loop, A);
  };

  module Subtractive = {
    module Medial_Magma = M.Medial_Magma(Int.Subtractive.Medial_Magma, A);
    module Quasigroup = M.Quasigroup(Int.Subtractive.Quasigroup, A);
  };

  module Eq = M.Eq(Int.Eq, A);
  module Ord = M.Ord(Int.Ord, A);
  module Bounded = M.Bounded(Int.Bounded, A);
  module Semiring = M.Semiring(Int.Semiring, A);
  module Ring = M.Ring(Int.Ring, A);
  module Commutative_Ring = M.Commutative_Ring(Int.Commutative_Ring, A);
  module Euclidean_Ring = M.Euclidean_Ring(Int.Euclidean_Ring, A);

  let suites =
    ListLabels.concat([
      [
        Additive.Medial_Magma.suite,
        Additive.Semigroup.suite,
        Additive.Monoid.suite,
        Additive.Quasigroup.suite,
        Additive.Loop.suite,
        Additive.Group.suite,
        Additive.Abelian_Group.suite,
      ]
      |> ListLabels.map(~f=suite => suite("Int.Additive")),
      [
        Multiplicative.Medial_Magma.suite,
        Multiplicative.Semigroup.suite,
        Multiplicative.Monoid.suite,
        Multiplicative.Quasigroup.suite,
        Multiplicative.Loop.suite,
      ]
      |> ListLabels.map(~f=suite => suite("Int.Multiplicative")),
      [Subtractive.Medial_Magma.suite, Subtractive.Quasigroup.suite]
      |> ListLabels.map(~f=suite => suite("Int.Subtractive")),
      [
        Eq.suite,
        Ord.suite,
        Bounded.suite,
        Semiring.suite,
        Ring.suite,
        Commutative_Ring.suite,
        Euclidean_Ring.suite,
      ]
      |> ListLabels.map(~f=suite => suite("Int")),
    ]);
};

module Option =
       (
         T: TEST,
         Q: QUICKCHECK with type t = T.test,
         A:
           ARBITRARY with
             type t = option(int) and type arbitrary('a) = Q.arbitrary('a),
         AA:
           ARBITRARY_A with
             type t('a) = option('a) and
             type arbitrary('a) = Q.arbitrary('a),
       ) => {
  module M = Make(T, Q);

  module Semigroup = M.Semigroup(Functors.OptionF.Int.Additive.Semigroup, A);
  module Monoid = M.Monoid(Functors.OptionF.Int.Additive.Monoid, A);
  module Functor = M.Functor(Option.Functor, AA);
  module Apply = M.Apply(Option.Applicative, AA);
  module Applicative = M.Applicative(Option.Applicative, AA);
  module Monad = M.Monad(Option.Monad, AA);
  module Alt = M.Alt(Option.Alt, AA);
  module Plus = M.Plus(Option.Plus, AA);
  module Alternative = M.Alternative(Option.Alternative, AA);
  module Eq = M.Eq(Functors.OptionF.Int.Eq, A);
  module Ord = M.Ord(Functors.OptionF.Int.Ord, A);

  let infix =
    T.suite(
      "Option.Infix",
      [
        T.test("should apply a default value if it's None", () => {
          let (|?) = Option.Infix.(|?);
          T.check(T.string, "foo" |? Some("bar"), "bar");
          T.check(T.string, "foo" |? None, "foo");
        }),
      ],
    );

  let foldable =
    T.suite(
      "Option.Foldable",
      [
        T.test("should do a left fold", () => {
          T.check(T.int, Option.Foldable.fold_left((+), 0, Some(1)), 1)
        }),
        T.test("should do a right fold", () => {
          T.check(T.int, Option.Foldable.fold_right((+), 0, Some(1)), 1);
          T.check(T.int, Option.Foldable.fold_right((+), 0, None), 0);
        }),
        T.test("should do a map fold (int)", () => {
          let fold_map = Functors.OptionF.Int.Additive.Fold_Map.(fold_map);
          T.check(T.int, fold_map(( * )(2), Some(3)), 6);
          T.check(T.int, fold_map((+)(1), None), 0);
        }),
        T.test("should do a map fold (list)", () => {
          let fold_map = Functors.OptionF.List.Fold_Map_Plus.(fold_map);
          T.check(T.list(T.int), fold_map(x => [x], Some(123)), [123]);
        }),
      ],
    );

  let traversable = {
    let (traverse, sequence) =
      Functors.OptionF.List.Traversable.(traverse, sequence);
    T.suite(
      "Option.Traversable",
      [
        T.test("should traverse the list", () => {
          let positive_int = x => x >= 0 ? [x] : [];
          T.check(
            T.list(T.option(T.int)),
            traverse(positive_int, Some(123)),
            [Some(123)],
          );
        }),
        T.test("should sequence the list", () => {
          T.check(
            T.list(T.option(T.int)),
            sequence(Some([3, 4, 5])),
            [Some(3), Some(4), Some(5)],
          );
          T.check(T.list(T.option(T.int)), sequence(None), [None]);
        }),
      ],
    );
  };

  let suites =
    [
      Semigroup.suite,
      Monoid.suite,
      Functor.suite,
      Apply.suite,
      Applicative.suite,
      Monad.suite,
      Alt.suite,
      Alternative.suite,
      Plus.suite,
      Eq.suite,
      Ord.suite,
    ]
    |> ListLabels.map(~f=suite => suite("Option"))
    |> ListLabels.append([infix, foldable, traversable]);
};

module String =
       (
         T: TEST,
         Q: QUICKCHECK with type t = T.test,
         A:
           ARBITRARY with
             type t = string and type arbitrary('a) = Q.arbitrary('a),
       ) => {
  module M = Make(T, Q);

  module Semigroup = M.Semigroup(String.Semigroup, A);
  module Monoid = M.Monoid(String.Monoid, A);
  module Quasigroup = M.Quasigroup(String.Quasigroup, A);
  module Loop = M.Loop(String.Loop, A);

  module Eq = M.Eq(String.Eq, A);
  module Ord = M.Ord(String.Ord, A);

  let suites =
    [
      Semigroup.suite,
      Monoid.suite,
      Quasigroup.suite,
      Loop.suite,
      Eq.suite,
      Ord.suite,
    ]
    |> ListLabels.map(~f=suite => suite("String"));
};
