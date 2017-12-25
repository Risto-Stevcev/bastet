open Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
module Fn = Infix.Semigroupoid(Function.Semigroupoid);

describe("Default", () => {
  module Foldable: Interface.FOLDABLE with type t('a) = list('a) = {
    type t('a) = list('a);

    module FM: Default.FOLD_MAP with type t('a) = list('a) = {
      type t('a) = list('a);
      module Fold_Map_Any = (M: Interface.MONOID_ANY) => {
        let fold_map = (f, x) =>
          ListLabels.fold_left(~f=(acc, x) => M.append(acc, f(x)), ~init=M.empty, x);
      };
    };

    module Fold_Map = List.Foldable.Fold_Map;
    module Fold_Map_Any = FM.Fold_Map_Any;
    module F = Default.Fold(FM);

    let fold_left = F.fold_left_default;
    let fold_right = F.fold_right_default;
  };

  module Traversable: List.TRAVERSABLE_F = (A: Interface.APPLICATIVE) => {
    type t('a) = list('a);
    type applicative_t('a) = A.t('a);
    include (List.Functor: Interface.FUNCTOR with type t('a) := t('a));
    include (List.Foldable: Interface.FOLDABLE with type t('a) := t('a));

    module I = Infix.Apply(A);
    let sequence = (xs) => I.({
      ListLabels.fold_right(
        ~f=(acc, x) => A.pure((y, ys) => [y, ...ys]) <*> acc <*> x,
        ~init=A.pure([]),
        xs
      )
    });

    module D = Default.Traverse({
      type t('a) = list('a);
      type applicative_t('a) = A.t('a);
      include (List.Functor: Interface.FUNCTOR with type t('a) := t('a));
      let sequence = sequence;
    });
    let traverse = D.traverse_default;
  };

  describe("Foldable", () => Foldable.({
    it("should do a left fold", () => {
      expect(fold_left((+), 0, [1,2,3,4,5])).to_be(15);
      expect(fold_left((-), 10, [3,2,1])).to_be(4);
    });
    it("should do a right fold", () => {
      expect(fold_right((-), 10, [3,2,1])).to_be(-8);
    });
  }));

  describe("Traversable", () => {
    module T = Traversable(Option.Applicative);

    it("should traverse the list", () => T.({
      let positive_int = (x) => x >= 0 ? Some(x) : None;
      expect(traverse(positive_int, [1,2,3])).to_be(Some([1,2,3]));
      expect(traverse(positive_int, [1,2,-3])).to_be(None);
    }));

    it("should sequence the list", () => T.({
      expect(sequence([Some(3), Some(4), Some(5)])).to_be(Some([3,4,5]));
      expect(sequence([Some(3), Some(4), None])).to_be(None);
    }));
  });
});

describe("Int", () => {
  let arb_int' = arb_int(-10000, 10000);

  describe("Additive", () => {
    describe("Semigroup", () => {
      module V = Verify.Semigroup(Int.Additive.Semigroup);
      property3("should satisfy associativity", arb_int', arb_int', arb_int', V.associativity)
    });

    describe("Monoid", () => {
      module V = Verify.Monoid(Int.Additive.Monoid);
      property1("should satisfy neutrality", arb_int', V.neutral)
    });
  });

  describe("Multiplicative", () => {
    describe("Semigroup", () => {
      module V = Verify.Semigroup(Int.Multiplicative.Semigroup);
      property3("should satisfy associativity", arb_int', arb_int', arb_int', V.associativity)
    });

    describe("Monoid", () => {
      module V = Verify.Monoid(Int.Multiplicative.Monoid);
      property1("should satisfy neutrality", arb_int', V.neutral)
    });
  })
});


describe("String", () => {
  describe("Semigroup", () => {
    module V = Verify.Semigroup(String.Semigroup);
    property3(
      "should satisfy associativity", arb_string, arb_string, arb_string, V.associativity
    )
  });

  describe("Monoid", () => {
    module V = Verify.Monoid(String.Monoid);
    property1("should satisfy neutrality", arb_string, V.neutral)
  });
});


describe("Bool", () => {
  describe("Conjunctive", () => {
    describe("Semigroup", () => {
      module V = Verify.Semigroup(Bool.Conjunctive.Semigroup);

      property3("should satisfy associativity", arb_bool, arb_bool, arb_bool, (a, b, c) => {
        let (a', b', c') = (a |> Js.to_bool, b |> Js.to_bool, c |> Js.to_bool);
        V.associativity(a', b', c')
      })
    });

    describe("Monoid", () => Fn.({
      module V = Verify.Monoid(Bool.Conjunctive.Monoid);
      property1("should satisfy neutrality", arb_bool, V.neutral << Js.to_bool)
    }));
  });

  describe("Disjunctive", () => {
    describe("Semigroup", () => {
      module V = Verify.Semigroup(Bool.Disjunctive.Semigroup);

      property3("should satisfy associativity", arb_bool, arb_bool, arb_bool, (a, b, c) => {
        let (a', b', c') = (a |> Js.to_bool, b |> Js.to_bool, c |> Js.to_bool);
        V.associativity(a', b', c')
      })
    });

    describe("Monoid", () => Fn.({
      module V = Verify.Monoid(Bool.Disjunctive.Monoid);
      property1("should satisfy neutrality", arb_bool, V.neutral << Js.to_bool)
    }));
  });
});

describe("Array", () => Fn.({
  describe("Semigroup", () => {
    module V = Verify.Semigroup_Any(Array.Semigroup);
    property3(
      "should satisfy associativity",
      arb_array(arb_nat), arb_array(arb_nat), arb_array(arb_nat),
      V.associativity
    )
  });

  describe("Monoid", () => {
    module V = Verify.Monoid_Any(Array.Monoid);
    property1("should satisfy neutrality", arb_array(arb_nat), V.neutral)
  });

  describe("Functor", () => {
    module V = Verify.Functor(Array.Functor);
    property1("should satisfy identity", arb_array(arb_nat), V.identity);
    property1("should satisfy composition", arb_array(arb_nat), (a) => {
      V.composition((++)("!"), string_of_int, a)
    })
  });

  describe("Apply", () => {
    module V = Verify.Apply(Array.Apply);
    property1("should satisfy associative composition", arb_array(arb_nat), (n) => {
      V.associative_composition([|(++)("!")|], [|string_of_int|], n)
    })
  });

  describe("Applicative", () => {
    module V = Verify.Applicative(Array.Applicative);
    property1("should satisfy identity", arb_array(arb_nat), V.identity);
    property1(
      "should satisfy homomorphism",
      arb_array(arb_nat),
      V.homomorphism(Array.Functor.map(string_of_int))
    );
    property1("should satisfy interchange", arb_nat, V.interchange([|string_of_int|]));
  });

  describe("Monad", () => {
    module V = Verify.Monad(Array.Monad);
    open Array.Applicative;
    property1(
      "should satisfy associativity",
      arb_array(arb_nat),
      V.associativity(pure << string_of_int, pure << (++)("!"))
    );
    property1(
      "should satisfy left identity", arb_nat, V.left_identity(pure << string_of_int)
    );
    property1(
      "should satisfy right identity", arb_array(arb_nat), V.right_identity
    );
  });


  describe("Alt", () => {
    module V = Verify.Alt(Array.Alt);
    property3(
      "should satisfy associativity",
      arb_array(arb_nat), arb_array(arb_nat), arb_array(arb_nat),
      V.associativity
    );
    property2(
      "should satisfy distributivity",
      arb_array(arb_nat), arb_array(arb_nat),
      V.distributivity(string_of_int)
    );
  });

  describe("Plus", () => {
    module V = Verify.Plus(Array.Plus);
    it("should satisfy annihalation", () => {
      expect(V.annihalation(string_of_int)).to_be(true);
    });
    property1("should satisfy left identity", arb_array(arb_nat), V.left_identity);
    property1("should satisfy right identity", arb_array(arb_nat), V.right_identity);
  });

  describe("Alternative", () => {
    module V = Verify.Alternative(Array.Alternative);
    let (pure) = Array.Applicative.((pure));
    property1(
      "should satisfy distributivity",
      arb_array(arb_nat),
      V.distributivity(pure((*)(3)), pure((+)(4)))
    );
    it("should satisfy annihalation", () => {
      expect(V.annihalation(Array.Applicative.pure(string_of_int))).to_be(true);
    });
  });

  describe("Foldable", () => Array.Foldable.({
    it("should do a left fold", () => {
      expect(fold_left((+), 0, [|1,2,3,4,5|])).to_be(15);
      expect(fold_left((-), 10, [|3,2,1|])).to_be(4);
    });

    it("should do a right fold", () => {
      expect(fold_right((-), 10, [|3,2,1|])).to_be(-8);
    });

    it("should do a map fold (int)", () => {
      module F = Array.Foldable.Fold_Map(Int.Additive.Monoid);
      expect(F.fold_map(Function.Category.id, [|1,2,3|])).to_be(6);
    });

    it("should do a map fold (list)", () => {
      module F = Array.Foldable.Fold_Map_Any(List.Monoid);
      expect(F.fold_map(List.Applicative.pure, [|[1,2,3],[4,5]|])).to_be([[1,2,3],[4,5]]);
    });
  }));

  describe("Traversable", () => {
    module T = Array.Traversable(Option.Applicative);

    it("should traverse the array", () => T.({
      let positive_int = (x) => x >= 0 ? Some(x) : None;
      expect(traverse(positive_int, [|1,2,3|])).to_be(Some([|1,2,3|]));
      expect(traverse(positive_int, [|1,2,-3|])).to_be(None);
    }));

    it("should sequence the array", () => T.({
      expect(sequence([|Some(3), Some(4), Some(5)|])).to_be(Some([|3,4,5|]));
      expect(sequence([|Some(3), Some(4), None|])).to_be(None);
    }));
  });
}));


describe("List", () => Fn.({
  let to_list = ArrayLabels.to_list;

  describe("Semigroup", () => {
    module V = Verify.Semigroup_Any(List.Semigroup);
    property3(
      "should satisfy associativity",
      arb_array(arb_nat), arb_array(arb_nat), arb_array(arb_nat),
      (a, b, c) => {
        let (a', b', c') = (to_list(a), to_list(b), to_list(c));
        V.associativity(a', b', c')
      }
    )
  });

  describe("Monoid", () => {
    module V = Verify.Monoid_Any(List.Monoid);
    property1("should satisfy neutrality", arb_array(arb_nat), V.neutral << to_list)
  });

  describe("Functor", () => {
    module V = Verify.Functor(List.Functor);

    property1("should satisfy identity", arb_array(arb_nat), V.identity << to_list);
    property1("should satisfy composition", arb_array(arb_nat), (a) => {
      V.composition((++)("!"), string_of_int, to_list(a))
    })
  });

  describe("Apply", () => {
    module V = Verify.Apply(List.Apply);
    property1("should satisfy associative composition", arb_nat, (n) => {
      V.associative_composition([(++)("!")], [string_of_int], [n])
    })
  });

  describe("Applicative", () => {
    module V = Verify.Applicative(List.Applicative);
    property1("should satisfy identity", arb_array(arb_nat), V.identity << to_list);
    property1(
      "should satisfy homomorphism",
      arb_array(arb_nat),
      V.homomorphism(List.Functor.map(string_of_int)) << to_list
    );
    property1("should satisfy interchange", arb_nat, V.interchange([string_of_int]));
  });

  describe("Monad", () => {
    module V = Verify.Monad(List.Monad);
    let (pure) = List.Applicative.((pure));
    property1(
      "should satisfy associativity",
      arb_array(arb_nat),
      V.associativity(pure << string_of_int, pure << (++)("!")) << to_list
    );
    property1(
      "should satisfy left identity", arb_nat, V.left_identity(pure << string_of_int)
    );
    property1(
      "should satisfy right identity", arb_array(arb_nat), V.right_identity << to_list
    );
  });

  describe("Alt", () => {
    module V = Verify.Alt(List.Alt);
    property3(
      "should satisfy associativity",
      arb_array(arb_nat), arb_array(arb_nat), arb_array(arb_nat),
      (a, b, c) => {
        let (a', b', c') = (to_list(a), to_list(b), to_list(c));
        V.associativity(a', b', c')
      }
    );
    property2(
      "should satisfy distributivity",
      arb_array(arb_nat), arb_array(arb_nat),
      (a, b) => V.distributivity(string_of_int, to_list(a), to_list(b))
    );
  });

  describe("Plus", () => {
    module V = Verify.Plus(List.Plus);
    it("should satisfy annihalation", () => {
      expect(V.annihalation(string_of_int)).to_be(true);
    });
    property1(
      "should satisfy left identity", arb_array(arb_nat), V.left_identity << to_list
    );
    property1(
      "should satisfy right identity", arb_array(arb_nat), V.right_identity << to_list
    );
  });

  describe("Alternative", () => {
    module V = Verify.Alternative(List.Alternative);
    let (pure) = List.Applicative.((pure));
    property1(
      "should satisfy distributivity",
      arb_array(arb_nat),
      V.distributivity(pure((*)(3)), pure((+)(4))) << to_list
    );
    it("should satisfy annihalation", () => {
      expect(V.annihalation(List.Applicative.pure(string_of_int))).to_be(true);
    });
  });

  describe("Foldable", () => List.Foldable.({
    it("should do a left fold", () => {
      expect(fold_left((+), 0, [1,2,3,4,5])).to_be(15);
      expect(fold_left((-), 10, [3,2,1])).to_be(4);
    });

    it("should do a right fold", () => {
      expect(fold_right((-), 10, [3,2,1])).to_be(-8);
    });

    it("should do a map fold (int)", () => {
      module F = List.Foldable.Fold_Map(Int.Additive.Monoid);
      expect(F.fold_map(Function.Category.id, [1,2,3])).to_be(6);
    });

    it("should do a map fold (list)", () => {
      module F = List.Foldable.Fold_Map_Any(List.Monoid);
      expect(F.fold_map(List.Applicative.pure, [[1,2,3],[4,5]])).to_be([[1,2,3],[4,5]]);
    });
  }));

  describe("Traversable", () => {
    module T = List.Traversable(Option.Applicative);

    it("should traverse the list", () => T.({
      let positive_int = (x) => x >= 0 ? Some(x) : None;
      expect(traverse(positive_int, [1,2,3])).to_be(Some([1,2,3]));
      expect(traverse(positive_int, [1,2,-3])).to_be(None);
    }));

    it("should sequence the list", () => T.({
      expect(sequence([Some(3), Some(4), Some(5)])).to_be(Some([3,4,5]));
      expect(sequence([Some(3), Some(4), None])).to_be(None);
    }));
  });
}));

describe("Function", () => Fn.({
  describe("Semigroupoid", () => {
    property1("should satisfy associativity", arb_nat, (n) => {
      let (a, b, c) = ((==)("123!"), (++)("!"), string_of_int);
      (a << b << c)(n) == (a << (b << c))(n)
    });
  });

  describe("Category", () => {
    property1("should satisfy identity", arb_nat, (n) => {
      (Function.Category.id << string_of_int)(n) == string_of_int(n) &&
      (string_of_int << Function.Category.id)(n) == string_of_int(n)
    });
  });
}));

describe("Option", () => Fn.({
  module Option_Semigroup = Option.Semigroup(Int.Additive.Semigroup);
  module Option_Monoid = Option.Monoid(Int.Additive.Semigroup);

  let option_from_tuple: (('a, Js.boolean)) => option('a) = (a) => {
    let (v, b) = a;
    b == Js.true_ ? Some(v) : None;
  };

  describe("Semigroup", () => {
    module V = Verify.Semigroup_Any(Option_Semigroup);

    property3(
      "should satisfy associativity",
      arb_tuple((arb_nat, arb_bool)),
      arb_tuple((arb_nat, arb_bool)),
      arb_tuple((arb_nat, arb_bool)),
      (a, b, c) =>
        V.associativity(option_from_tuple(a), option_from_tuple(b), option_from_tuple(c))
    )
  });

  describe("Monoid", () => {
    module V = Verify.Monoid_Any(Option_Monoid);
    property1(
      "should satisfy neutrality",
      arb_tuple((arb_nat, arb_bool)),
      V.neutral << option_from_tuple
    )
  });

  describe("Functor", () => {
    module V = Verify.Functor(Option.Functor);
    property1(
      "should satisfy identity",
      arb_tuple((arb_nat, arb_bool)),
      V.identity << option_from_tuple
    );
    property1(
      "should satisfy composition",
      arb_tuple((arb_nat, arb_bool)),
      V.composition((++)("!"), string_of_int) << option_from_tuple
    );
  });

  describe("Apply", () => {
    module V = Verify.Apply(Option.Apply);
    property1(
      "should satisfy associative composition",
      arb_tuple((arb_nat, arb_bool)),
      V.associative_composition(Some((++)("!")), Some(string_of_int)) << option_from_tuple
    )
  });

  describe("Applicative", () => {
    module V = Verify.Applicative(Option.Applicative);
    property1(
      "should satisfy identity",
      arb_tuple((arb_nat, arb_bool)),
      V.identity << option_from_tuple
    );
    property1("should satisfy homomorphism", arb_nat, V.homomorphism(string_of_int));
    property1("should satisfy interchange", arb_nat, V.interchange(Some(string_of_int)));
  });

  describe("Monad", () => {
    module V = Verify.Monad(Option.Monad);
    open Option.Applicative;
    property1(
      "should satisfy associativity",
      arb_tuple((arb_nat, arb_bool)),
      V.associativity(pure << string_of_int, pure << (++)("!")) << option_from_tuple
    );
    property1(
      "should satisfy left identity", arb_nat, V.left_identity(pure << string_of_int)
    );
    property1(
      "should satisfy right identity",
      arb_tuple((arb_nat, arb_bool)),
      V.right_identity << option_from_tuple
    );
  });

  describe("Foldable", () => Option.Foldable.({
    it("should do a left fold", () => {
      expect(fold_left((+), 0, Some(1))).to_be(1);
    });

    it("should do a right fold", () => {
      expect(fold_right((+), 0, Some(1))).to_be(1);
      expect(fold_right((+), 0, None)).to_be(0);
    });

    it("should do a map fold (int)", () => {
      module F = Option.Foldable.Fold_Map(Int.Additive.Monoid);
      expect(F.fold_map((*)(2), Some(3))).to_be(6);
      expect(F.fold_map((+)(1), None)).to_be(Int.Additive.Monoid.empty);
    });

    it("should do a map fold (list)", () => {
      module F = Option.Foldable.Fold_Map_Any(List.Monoid);
      expect(F.fold_map(List.Applicative.pure, Some(123))).to_be([123]);
    });
  }));

  describe("Traversable", () => {
    module T = Option.Traversable(List.Applicative);

    it("should traverse the list", () => T.({
      let positive_int = (x) => x >= 0 ? [x] : [];
      expect(traverse(positive_int, Some(123))).to_be([Some(123)]);
      expect(traverse(positive_int, Some(-123))).to_be([]);
    }));

    it("should sequence the list", () => T.({
      expect(sequence(Some([3, 4, 5]))).to_be([Some(3), Some(4), Some(5)]);
      expect(sequence(None)).to_be([None]);
    }));
  });
}));


describe("Endo", () => {
  ()
});


describe("Dual", () => {
  ()
});


describe("Functions", () => {
  describe("Traversable", () => {
    module List_Traversable = Functions.Traversable(List.Traversable);
    module Array_Traversable  = Functions.Traversable(Array.Traversable);

    describe("Scan", () => {
      module List_Scan = List_Traversable.Scan({type s = int});
      module Array_Scan = Array_Traversable.Scan({type s = int});

      describe("scan_left", () => {
        it("should scan from the left (list)", () => List_Scan.({
          expect(scan_left((+), 0, [1,2,3])).to_be([1,3,6]);
          expect(scan_left((-), 10, [1,2,3])).to_be([9,7,4]);
        }));

        it("should scan from the left (array)", () => Array_Scan.({
          expect(scan_left((+), 0, [|1,2,3|])).to_be([|1,3,6|]);
          expect(scan_left((-), 10, [|1,2,3|])).to_be([|9,7,4|]);
        }));
      });

      describe("scan_right", () => {
        it("should scan from the right (list)", () => List_Scan.({
          expect(scan_right((+), 0, [1,2,3])).to_be([6,5,3]);
          expect(scan_right(Function.flip((-)), 10, [1,2,3])).to_be([4,5,7]);
        }));

        it("should scan from the right (array)", () => Array_Scan.({
          expect(scan_right((+), 0, [|1,2,3|])).to_be([|6,5,3|]);
          expect(scan_right(Function.flip((-)), 10, [|1,2,3|])).to_be([|4,5,7|]);
        }));
      });
    });
  });
});
