open BsMocha.Mocha;
open BsChai.Expect.Expect;
open BsChai.Expect.Combos.End;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
open Functors;
let (<.) = Function.Infix.(<.);
let (|?) = Option.Infix.(|?);

describe("Option", () => {
  describe("Infix.(|?)", () =>
    it("should apply a default value if it's None", () => {
      expect("foo" |? Some("bar")) |> to_be("bar");
      expect("foo" |? None) |> to_be("foo");
    })
  );

  describe("Semigroup", () => {
    module V = Verify.Semigroup(OptionF.Int.Additive.Semigroup);
    property3(
      "should satisfy associativity",
      arb_option(arb_nat),
      arb_option(arb_nat),
      arb_option(arb_nat),
      V.associativity,
    );
  });

  describe("Monoid", () => {
    module V = Verify.Monoid(OptionF.Int.Additive.Monoid);
    property1("should satisfy identity", arb_option(arb_nat), V.identity);
  });

  describe("Functor", () => {
    module V = Verify.Functor(Option.Functor);
    property1("should satisfy identity", arb_option(arb_nat), V.identity);
    property1(
      "should satisfy composition",
      arb_option(arb_nat),
      V.composition((++)("!"), string_of_int),
    );
  });
  describe("Apply", () => {
    module V = Verify.Apply(Option.Apply);
    property1(
      "should satisfy associative composition",
      arb_option(arb_nat),
      V.associative_composition(Some((++)("!")), Some(string_of_int)),
    );
  });

  describe("Applicative", () => {
    module V = Verify.Applicative(Option.Applicative);
    property1("should satisfy identity", arb_option(arb_nat), V.identity);
    property1(
      "should satisfy homomorphism",
      arb_nat,
      V.homomorphism(string_of_int),
    );
    property1(
      "should satisfy interchange",
      arb_nat,
      V.interchange(Some(string_of_int)),
    );
  });

  describe("Monad", () => {
    module V = Verify.Monad(Option.Monad);
    let pure = Option.Applicative.(pure);
    property1(
      "should satisfy associativity",
      arb_option(arb_nat),
      V.associativity(pure <. string_of_int, pure <. (++)("!")),
    );
    property1(
      "should satisfy identity",
      arb_nat,
      V.identity(pure <. string_of_int),
    );
  });

  describe("Alt", () => {
    module V = Verify.Alt(Option.Alt);
    property3(
      "should satisfy associativity",
      arb_option(arb_nat),
      arb_option(arb_nat),
      arb_option(arb_nat),
      V.associativity,
    );
    property2(
      "should satisfy distributivity",
      arb_option(arb_nat),
      arb_option(arb_nat),
      V.distributivity(string_of_int),
    );
  });

  describe("Plus", () => {
    module V = Verify.Plus(Option.Plus);
    it("should satisfy annihalation", () =>
      expect(V.annihalation(string_of_int)) |> to_be(true)
    );
    property1("should satisfy identity", arb_option(arb_nat), V.identity);
  });

  describe("Alternative", () => {
    open Option.Alternative;
    module V = Verify.Alternative(Option.Alternative);
    property1(
      "should satisfy distributivity",
      arb_option(arb_nat),
      V.distributivity(pure(( * )(2)), pure((+)(3))),
    );
    it("should satisfy annihalation", () =>
      expect(V.annihalation(string_of_int |> pure)) |> to_be(true)
    );
  });

  describe("Foldable", () => {
    let (fold_left, fold_right) = Option.Foldable.(fold_left, fold_right);
    it("should do a left fold", () =>
      expect(fold_left((+), 0, Some(1))) |> to_be(1)
    );
    it("should do a right fold", () => {
      expect(fold_right((+), 0, Some(1))) |> to_be(1);
      expect(fold_right((+), 0, None)) |> to_be(0);
    });
    it("should do a map fold (int)", () => {
      let fold_map = OptionF.Int.Additive.Fold_Map.(fold_map);
      expect(fold_map(( * )(2), Some(3))) |> to_be(6);
      expect(fold_map((+)(1), None)) |> to_be(Int.Additive.Monoid.empty);
    });
    it("should do a map fold (list)", () => {
      let fold_map = OptionF.List.Fold_Map_Plus.(fold_map);
      expect(fold_map(List.Applicative.pure, Some(123))) |> to_be([123]);
    });
  });

  describe("Traversable", () => {
    let (traverse, sequence) = OptionF.List.Traversable.(traverse, sequence);
    it("should traverse the list", () => {
      let positive_int = x => x >= 0 ? [x] : [];
      expect(traverse(positive_int, Some(123))) |> to_be([Some(123)]);
      expect(traverse(positive_int, Some(-123))) |> to_be([]);
    });
    it("should sequence the list", () => {
      expect(sequence(Some([3, 4, 5])))
      |> to_be([Some(3), Some(4), Some(5)]);
      expect(sequence(None)) |> to_be([None]);
    });
  });

  describe("Eq", () => {
    module V = Verify.Eq(OptionF.Int.Eq);
    property1(
      "should satisfy reflexivity",
      arb_option(arb_nat),
      V.reflexivity,
    );
    property2(
      "should satisfy symmetry",
      arb_option(arb_nat),
      arb_option(arb_nat),
      V.symmetry,
    );
    property3(
      "should satisfy transitivity",
      arb_option(arb_nat),
      arb_option(arb_nat),
      arb_option(arb_nat),
      V.transitivity,
    );
  });
});
