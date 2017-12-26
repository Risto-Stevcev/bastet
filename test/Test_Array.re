open Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
open Functors;
let (<.) = Function.Infix.(<.);


describe("Array", () => {
  describe("Functions", () => {
    describe("zip_with", () => {
      it("should zip two arrays", () => {
        expect(Array.zip_with((a, b) => (a, b), [|1,2,3|], [|"a","b","c"|]))
          .to_be([|(1,"a"), (2, "b"), (3, "c")|]);

        expect(Array.zip_with((*), [|1,2,3|], [|4,5,6|])).to_be([|4,10,18|]);
      })
    });

    describe("zip", () => {
      it("should zip two arrays", () => {
        expect(Array.zip([|1,2,3|], [|"a","b","c"|]))
          .to_be([|(1,"a"), (2, "b"), (3, "c")|]);
      })
    });
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
    let pure = Array.Applicative.pure;
    property1(
      "should satisfy associativity",
      arb_array(arb_nat),
      V.associativity(pure <. string_of_int, pure <. (++)("!"))
    );
    property1(
      "should satisfy left identity", arb_nat, V.left_identity(pure <. string_of_int)
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
    let pure = Array.Applicative.pure;
    property1(
      "should satisfy distributivity",
      arb_array(arb_nat),
      V.distributivity(pure((*)(3)), pure((+)(4)))
    );
    it("should satisfy annihalation", () => {
      expect(V.annihalation(pure(string_of_int))).to_be(true);
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
      let fold_map = ArrayF.Int.Additive.Fold_Map.fold_map;
      expect(fold_map(Function.Category.id, [|1,2,3|])).to_be(6);
    });

    it("should do a map fold (list)", () => {
      let fold_map = ArrayF.List.Fold_Map_Plus.fold_map;
      expect(fold_map(List.Applicative.pure, [|[1,2,3],[4,5]|])).to_be([[1,2,3],[4,5]]);
    });
  }));

  describe("Traversable", () => {
    let (traverse, sequence) = ArrayF.Option.Traversable.(traverse, sequence);
    it("should traverse the array", () => {
      let positive_int = (x) => x >= 0 ? Some(x) : None;
      expect(traverse(positive_int, [|1,2,3|])).to_be(Some([|1,2,3|]));
      expect(traverse(positive_int, [|1,2,-3|])).to_be(None);
    });
    it("should sequence the array", () => {
      expect(sequence([|Some(3), Some(4), Some(5)|])).to_be(Some([|3,4,5|]));
      expect(sequence([|Some(3), Some(4), None|])).to_be(None);
    });
  });

  describe("Eq", () => {
    let arb_int' = arb_int(-10000, 10000);
    module V = Verify.Eq(ArrayF.Int.Eq);
    property1("should satisfy reflexivity", arb_array(arb_int'), V.reflexivity);
    property2(
      "should satisfy symmetry", arb_array(arb_int'), arb_array(arb_int'), V.symmetry
    );
    property3(
      "should satisfy transitivity",
      arb_array(arb_int'), arb_array(arb_int'), arb_array(arb_int'),
      V.transitivity
    );
  });
});
