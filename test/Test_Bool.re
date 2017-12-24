open Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
module Fn = Infix.Semigroupoid(Function.Semigroupoid);

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
  describe("Default", () => {
    module Foldable: Interface.FOLDABLE with type t('a) = array('a) = {
      type t('a) = array('a);

      module FM: Default.FOLD_MAP with type t('a) = array('a) = {
        type t('a) = array('a);
        module Fold_Map_Any = (M: Interface.MONOID_ANY) => {
          let fold_map = (f, x) =>
            ArrayLabels.fold_left(~f=(acc, x) => M.append(acc, f(x)), ~init=M.empty, x);
        };
      };

      module Fold_Map = Array.Foldable.Fold_Map;
      module Fold_Map_Any = FM.Fold_Map_Any;
      module F = Default.Fold(FM);

      let fold_left = F.fold_left_default;
      let fold_right = F.fold_right_default;
    };

    describe("Foldable", () => Foldable.({
      it("should do a left fold", () => {
        expect(fold_left((+), 0, [|1,2,3,4,5|])).to_be(15);
        expect(fold_left((-), 10, [|3,2,1|])).to_be(4);
      });
      it("should do a right fold", () => {
        expect(fold_right((-), 10, [|3,2,1|])).to_be(-8);
      });
    }));
  });

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

  describe("Foldable", () => Array.Foldable.({
    it("should do a left fold", () => {
      expect(fold_left((+), 0, [|1,2,3,4,5|])).to_be(15);
      expect(fold_left((-), 10, [|3,2,1|])).to_be(4);
    });
    it("should do a right fold", () => {
      expect(fold_right((-), 10, [|3,2,1|])).to_be(-8);
    });
  }));

  describe("Fold_Map", () => {
    it("should do a map fold (int)", () => {
      module F = Array.Foldable.Fold_Map(Int.Additive.Monoid);
      expect(F.fold_map(Function.Category.id, [|1,2,3|])).to_be(6);
    });

    it("should do a map fold (list)", () => {
      module F = Array.Foldable.Fold_Map_Any(List.Monoid);
      expect(F.fold_map(List.Applicative.pure, [|[1,2,3],[4,5]|])).to_be([[1,2,3],[4,5]]);
    });
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
    })
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
    open List.Applicative;
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

  describe("Foldable", () => List.Foldable.({
    it("should do a left fold", () => {
      expect(fold_left((+), 0, [1,2,3,4,5])).to_be(15);
      expect(fold_left((-), 10, [3,2,1])).to_be(4);
    });
    it("should do a right fold", () => {
      expect(fold_right((-), 10, [3,2,1])).to_be(-8);
    });
  }));

  describe("Fold_Map", () => {
    it("should do a map fold (int)", () => {
      module F = List.Foldable.Fold_Map(Int.Additive.Monoid);
      expect(F.fold_map(Function.Category.id, [1,2,3])).to_be(6);
    });

    it("should do a map fold (list)", () => {
      module F = List.Foldable.Fold_Map_Any(List.Monoid);
      expect(F.fold_map(List.Applicative.pure, [[1,2,3],[4,5]])).to_be([[1,2,3],[4,5]]);
    });
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
}));
