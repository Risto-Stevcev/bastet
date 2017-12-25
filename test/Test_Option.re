open Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
module Fn = Infix.Semigroupoid(Function.Semigroupoid);


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

  describe("Eq", () => {
    module Option_Int_Eq = Option.Eq(Int.Eq);
    module V = Verify.Eq1(Option_Int_Eq);

    property1(
      "should satisfy reflexivity",
      arb_tuple((arb_nat, arb_bool)),
      V.reflexivity << option_from_tuple
    );
    property2(
      "should satisfy symmetry",
      arb_tuple((arb_nat, arb_bool)),
      arb_tuple((arb_nat, arb_bool)),
      (a, b) =>
        V.symmetry(option_from_tuple(a), option_from_tuple(b))
    );
    property3(
      "should satisfy transitivity",
      arb_tuple((arb_nat, arb_bool)),
      arb_tuple((arb_nat, arb_bool)),
      arb_tuple((arb_nat, arb_bool)),
      (a, b, c) =>
        V.transitivity(option_from_tuple(a), option_from_tuple(b), option_from_tuple(c))
    );
  });
}));
