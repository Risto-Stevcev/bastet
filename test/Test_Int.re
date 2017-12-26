open Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;


describe("Int", () => {
  let arb_int' = arb_int(-10000, 10000);

  describe("Additive", () => {
    describe("Semigroup", () => {
      module V = Verify.Semigroup(Int.Additive.Semigroup);
      property3(
        "should satisfy associativity", arb_int', arb_int', arb_int', V.associativity
      )
    });

    describe("Monoid", () => {
      module V = Verify.Monoid(Int.Additive.Monoid);
      property1("should satisfy neutrality", arb_int', V.neutral)
    });
  });

  describe("Multiplicative", () => {
    describe("Semigroup", () => {
      module V = Verify.Semigroup(Int.Multiplicative.Semigroup);
      property3(
        "should satisfy associativity", arb_int', arb_int', arb_int', V.associativity
      )
    });

    describe("Monoid", () => {
      module V = Verify.Monoid(Int.Multiplicative.Monoid);
      property1("should satisfy neutrality", arb_int', V.neutral)
    });
  });

  describe("Eq", () => {
    module V = Verify.Eq(Int.Eq);
    property1("should satisfy reflexivity", arb_int', V.reflexivity);
    property2("should satisfy symmetry", arb_int', arb_int', V.symmetry);
    property3("should satisfy transitivity", arb_int', arb_int', arb_int', V.transitivity);
  });
});
