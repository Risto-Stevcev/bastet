open Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;


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

  describe("Eq", () => {
    module V = Verify.Eq(String.Eq);
    property1("should satisfy reflexivity", arb_string, V.reflexivity);
    property2("should satisfy symmetry", arb_string, arb_string, V.symmetry);
    property3(
      "should satisfy transitivity", arb_string, arb_string, arb_string, V.transitivity
    );
  });
});
