open BsMochajs.Mocha;
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
    property1("should satisfy identity", arb_string, V.identity)
  });

  describe("Quasigroup", () => {
    module V = Verify.Quasigroup(String.Quasigroup);
    property3(
      "should satisfy cancellative",
      arb_string, arb_string, arb_string,
      V.cancellative
    );
  });
  describe("Loop", () => {
    module V = Verify.Loop(String.Loop);
    property1("should satisfy identity", arb_string, V.identity)
  });

  describe("Eq", () => {
    module V = Verify.Eq(String.Eq);
    property1("should satisfy reflexivity", arb_string, V.reflexivity);
    property2("should satisfy symmetry", arb_string, arb_string, V.symmetry);
    property3(
      "should satisfy transitivity", arb_string, arb_string, arb_string, V.transitivity
    );
  });

  describe("Ord", () => {
    module V = Verify.Ord(String.Ord);
    property1("should satisfy reflexivity", arb_string, V.reflexivity);
    property2("should satisfy antisymmetry", arb_string, arb_string, V.antisymmetry);
    property3(
      "should satisfy transitivity", arb_string, arb_string, arb_string, V.transitivity
    );
  });
});
