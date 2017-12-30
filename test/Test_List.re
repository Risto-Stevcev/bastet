open Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
open Functors;
let (<.) = Function.Infix.(<.);


describe("List", () => {
  let to_list = ArrayLabels.to_list;

  describe("Functor", () => {
    module V = Verify.Functor(List.Functor);
    property1("should satisfy identity", arb_array(arb_nat), V.identity <. to_list);
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
    property1("should satisfy identity", arb_array(arb_nat), V.identity <. to_list);
    property1(
      "should satisfy homomorphism",
      arb_array(arb_nat),
      V.homomorphism(List.Functor.map(string_of_int)) <. to_list
    );
    property1("should satisfy interchange", arb_nat, V.interchange([string_of_int]));
  });

  describe("Monad", () => {
    module V = Verify.Monad(List.Monad);
    let (pure) = List.Applicative.((pure));
    property1(
      "should satisfy associativity",
      arb_array(arb_nat),
      V.associativity(pure <. string_of_int, pure <. (++)("!")) <. to_list
    );
    property1("should satisfy identity", arb_nat, V.identity(pure <. string_of_int));
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
      "should satisfy identity", arb_array(arb_nat), V.identity <. to_list
    );
  });

  describe("Alternative", () => {
    module V = Verify.Alternative(List.Alternative);
    let (pure) = List.Applicative.((pure));
    property1(
      "should satisfy distributivity",
      arb_array(arb_nat),
      V.distributivity(pure((*)(3)), pure((+)(4))) <. to_list
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
      let fold_map = ListF.Int.Additive.Fold_Map.fold_map;
      expect(fold_map(Function.Category.id, [1,2,3])).to_be(6);
    });

    it("should do a map fold (list)", () => {
      let fold_map = ListF.List.Fold_Map_Plus.fold_map;
      expect(fold_map(List.Applicative.pure, [[1,2,3],[4,5]])).to_be([[1,2,3],[4,5]]);
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

  describe("Eq", () => {
    module V = Verify.Eq(ListF.Int.Eq);
    let arb_int' = arb_int(-10000, 10000);

    property1("should satisfy reflexivity", arb_array(arb_int'), V.reflexivity <. to_list);
    property2(
      "should satisfy symmetry",
      arb_array(arb_int'), arb_array(arb_int'),
      (a, b) => V.symmetry(to_list(a), to_list(b))
    );
    property3(
      "should satisfy transitivity",
      arb_array(arb_int'), arb_array(arb_int'), arb_array(arb_int'),
      (a, b, c) => V.transitivity(to_list(a), to_list(b), to_list(c))
    );
  });


  describe("Show", () => {
    module S = List.Show(Int.Show);
    it("should convert the array to a string", () => {
      expect(S.show([1, 1, 2, 3, 5, 8, 13])).to_be("[1, 1, 2, 3, 5, 8, 13]")
    });
  });
});
