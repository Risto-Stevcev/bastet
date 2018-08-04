open BsMocha.Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
let (<.) = Function.Infix.(<.);

describe("JsDate", () => {
  describe("Medial Magma", () => {
    module V = Verify.Medial_Magma(JsDate.Medial_Magma);
    property4(
      "should satisfy bicommutativity",
      arb_date,
      arb_date,
      arb_date,
      arb_date,
      V.bicommutativity,
    );
  });

  describe("Semigroup", () => {
    module V = Verify.Semigroup(JsDate.Semigroup);
    property3(
      "should satisfy associativity",
      arb_date,
      arb_date,
      arb_date,
      V.associativity,
    );
  });

  describe("Monoid", () => {
    module V = Verify.Monoid(JsDate.Monoid);
    property1("should satisfy identity", arb_date, V.identity);
  });

  describe("Eq", () => {
    module V = Verify.Eq(JsDate.Eq);
    property1("should satisfy reflexivity", arb_date, V.reflexivity);
    property2("should satisfy symmetry", arb_date, arb_date, V.symmetry);
    property3(
      "should satisfy transitivity",
      arb_date,
      arb_date,
      arb_date,
      V.transitivity,
    );
  });

  describe("Ord", () => {
    module V = Verify.Ord(JsDate.Ord);
    property1("should satisfy reflexivity", arb_date, V.reflexivity);
    property2(
      "should satisfy antisymmetry",
      arb_date,
      arb_date,
      V.antisymmetry,
    );
    property3(
      "should satisfy transitivity",
      arb_date,
      arb_date,
      arb_date,
      V.transitivity,
    );
  });
});
