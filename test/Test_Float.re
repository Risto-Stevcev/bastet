open Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;


describe("Float", () => {
  let arb_float' = arb_float(-10000.0, 10000.0);

  describe("Additive", () => {
    describe("Semigroup", () => {
      module V = Verify.Semigroup(Float.Additive.Semigroup);
      property3(
        "should satisfy associativity", arb_float', arb_float', arb_float', V.associativity
      )
    });

    describe("Monoid", () => {
      module V = Verify.Monoid(Float.Additive.Monoid);
      property1("should satisfy neutrality", arb_float', V.neutral)
    });
  });

  describe("Multiplicative", () => {
    describe("Semigroup", () => {
      module V = Verify.Semigroup(Float.Multiplicative.Semigroup);
      property3(
        "should satisfy associativity", arb_float', arb_float', arb_float', V.associativity
      )
    });

    describe("Monoid", () => {
      module V = Verify.Monoid(Float.Multiplicative.Monoid);
      property1("should satisfy neutrality", arb_float', V.neutral)
    });
  });

  describe("Eq", () => {
    module V = Verify.Eq(Float.Eq);
    property1("should satisfy reflexivity", arb_float', V.reflexivity);
    property2("should satisfy symmetry", arb_float', arb_float', V.symmetry);
    property3(
      "should satisfy transitivity", arb_float', arb_float', arb_float', V.transitivity
    );
  });

  describe("Ord", () => {
    module V = Verify.Ord(Float.Ord);
    property1("should satisfy reflexivity", arb_float', V.reflexivity);
    property2("should satisfy antisymmetry", arb_float', arb_float', V.antisymmetry);
    property3(
      "should satisfy transitivity", arb_float', arb_float', arb_float', V.transitivity
    );
  });

  describe("Bounded", () => {
    module V = Verify.Bounded(Float.Bounded);
    property1("should satisfy bounded", arb_float', V.bounded);
  });
});
