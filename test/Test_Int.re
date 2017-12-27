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

  describe("Ord", () => {
    module V = Verify.Ord(Int.Ord);
    property1("should satisfy reflexivity", arb_int', V.reflexivity);
    property2("should satisfy antisymmetry", arb_int', arb_int', V.antisymmetry);
    property3("should satisfy transitivity", arb_int', arb_int', arb_int', V.transitivity);
  });

  describe("Bounded", () => {
    module V = Verify.Bounded(Int.Bounded);
    property1("should satisfy bounded", arb_int', V.bounded);
  });

  describe("Semiring", () => {
    module V = Verify.Semiring(Int.Semiring);
    property3(
      "should satisfy additive associativity",
      arb_int', arb_int', arb_int',
      V.additive_associativity
    );
    property1("should satisfy additive identity", arb_int', V.additive_identity);
    property2("should satisfy commutativity", arb_int', arb_int', V.commutativity);
    property3(
      "should satisfy multiplicative associativity",
      arb_int', arb_int', arb_int',
      V.multiplicative_associativity
    );
    property1(
      "should satisfy multiplicative identity", arb_int', V.multiplicative_identity
    );
    property3(
      "should satisfy left distributivity",
      arb_int', arb_int', arb_int',
      V.left_distributivity
    );
     property3(
      "should satisfy right distributivity",
      arb_int', arb_int', arb_int',
      V.right_distributivity
    );
  });
});
