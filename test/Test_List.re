open BsMocha.Mocha;
open BsChai.Expect.Expect;
open BsChai.Expect.Combos.End;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
open Functors;
let (<.) = Function.Infix.(<.);

describe("List", () => {
  describe("Semigroup_Any", () => {
    module V = Verify.Semigroup_Any(List.Semigroup_Any);
    property3(
      "should satisfy associativity",
      arb_list(arb_nat),
      arb_list(arb_nat),
      arb_list(arb_nat),
      V.associativity,
    );
  });

  describe("Monoid_Any", () => {
    module V = Verify.Monoid_Any(List.Monoid_Any);
    property1("should satisfy identity", arb_list(arb_nat), V.identity);
  });

  describe("Functor", () => {
    module V = Verify.Functor(List.Functor);
    property1("should satisfy identity", arb_list(arb_nat), V.identity);
    property1(
      "should satisfy composition",
      arb_list(arb_nat),
      V.composition((++)("!"), string_of_int),
    );
  });

  describe("Apply", () => {
    module V = Verify.Apply(List.Apply);
    property1("should satisfy associative composition", arb_nat, n =>
      V.associative_composition([(++)("!")], [string_of_int], [n])
    );
  });

  describe("Applicative", () => {
    module V = Verify.Applicative(List.Applicative);
    property1("should satisfy identity", arb_list(arb_nat), V.identity);
    property1(
      "should satisfy homomorphism",
      arb_list(arb_nat),
      V.homomorphism(List.Functor.map(string_of_int)),
    );
    property1(
      "should satisfy interchange",
      arb_nat,
      V.interchange([string_of_int]),
    );
  });

  describe("Monad", () => {
    module V = Verify.Monad(List.Monad);
    let pure = List.Applicative.(pure);
    property1(
      "should satisfy associativity",
      arb_list(arb_nat),
      V.associativity(pure <. string_of_int, pure <. (++)("!")),
    );
    property1(
      "should satisfy identity",
      arb_nat,
      V.identity(pure <. string_of_int),
    );
  });

  describe("Alt", () => {
    module V = Verify.Alt(List.Alt);
    property3(
      "should satisfy associativity",
      arb_list(arb_nat),
      arb_list(arb_nat),
      arb_list(arb_nat),
      V.associativity,
    );
    property2(
      "should satisfy distributivity",
      arb_list(arb_nat),
      arb_list(arb_nat),
      V.distributivity(string_of_int),
    );
  });

  describe("Plus", () => {
    module V = Verify.Plus(List.Plus);
    it("should satisfy annihalation", () =>
      expect(V.annihalation(string_of_int)) |> to_be(true)
    );
    property1("should satisfy identity", arb_list(arb_nat), V.identity);
  });

  describe("Alternative", () => {
    module V = Verify.Alternative(List.Alternative);
    let pure = List.Applicative.(pure);
    property1(
      "should satisfy distributivity",
      arb_list(arb_nat),
      V.distributivity(pure(( * )(3)), pure((+)(4))),
    );
    it("should satisfy annihalation", () =>
      expect(V.annihalation(List.Applicative.pure(string_of_int)))
      |> to_be(true)
    );
  });

  describe("Foldable", () => {
    open List.Foldable;
    it("should do a left fold", () => {
      expect(fold_left((+), 0, [1, 2, 3, 4, 5])) |> to_be(15);
      expect(fold_left((-), 10, [3, 2, 1])) |> to_be(4);
    });

    it("should do a right fold", () =>
      expect(fold_right((-), 10, [3, 2, 1])) |> to_be(-8)
    );

    it("should do a map fold (int)", () => {
      let fold_map = ListF.Int.Additive.Fold_Map.fold_map;
      expect(fold_map(Function.Category.id, [1, 2, 3])) |> to_be(6);
    });

    it("should do a map fold (list)", () => {
      let fold_map = ListF.List.Fold_Map_Plus.fold_map;
      expect(fold_map(List.Applicative.pure, [[1, 2, 3], [4, 5]]))
      |> to_be([[1, 2, 3], [4, 5]]);
    });
  });

  describe("Traversable", () => {
    module T = List.Traversable(Option.Applicative);

    it("should traverse the list", () => {
      open T;
      let positive_int = x => x >= 0 ? Some(x) : None;
      expect(traverse(positive_int, [1, 2, 3])) |> to_be(Some([1, 2, 3]));
      expect(traverse(positive_int, [1, 2, (-3)])) |> to_be(None);
    });
    it("should sequence the list", () => {
      open T;
      expect(sequence([Some(3), Some(4), Some(5)]))
      |> to_be(Some([3, 4, 5]));
      expect(sequence([Some(3), Some(4), None])) |> to_be(None);
    });
  });

  describe("Eq", () => {
    module V = Verify.Eq(ListF.Int.Eq);
    let arb_int' = arb_int(-10000, 10000);

    property1(
      "should satisfy reflexivity",
      arb_list(arb_int'),
      V.reflexivity,
    );
    property2(
      "should satisfy symmetry",
      arb_list(arb_int'),
      arb_list(arb_int'),
      V.symmetry,
    );
    property3(
      "should satisfy transitivity",
      arb_list(arb_int'),
      arb_list(arb_int'),
      arb_list(arb_int'),
      V.transitivity,
    );
  });

  describe("Show", () => {
    module S = List.Show(Int.Show);
    it("should convert the array to a string", () =>
      expect(S.show([1, 1, 2, 3, 5, 8, 13]))
      |> to_be("[1, 1, 2, 3, 5, 8, 13]")
    );
  });
});
