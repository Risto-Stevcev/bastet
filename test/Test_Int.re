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
    describe("Quasigroup", () => {
      module V = Verify.Quasigroup(Int.Additive.Quasigroup);
      property3(
        "should satisfy left cancellative",
        arb_int', arb_int', arb_int',
        V.left_cancellative
      );
      property3(
        "should satisfy right cancellative",
        arb_int', arb_int', arb_int',
        V.right_cancellative
      );
    });
    describe("Loop", () => {
      module V = Verify.Loop(Int.Additive.Loop);
      property1("should satisfy identity", arb_int', V.identity)
    });
    describe("Group", () => {
      module V = Verify.Group(Int.Additive.Group);
      property1("should satisfy invertibility", arb_int', V.invertibility);
      property3(
        "should satisfy associativity", arb_int', arb_int', arb_int', V.associativity
      )
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
    describe("Quasigroup", () => {
      module V = Verify.Quasigroup(Int.Multiplicative.Quasigroup);
      property3(
        "should satisfy left cancellative",
        arb_int', arb_int', arb_int',
        V.left_cancellative
      );
      property3(
        "should satisfy right cancellative",
        arb_int', arb_int', arb_int',
        V.right_cancellative
      );
    });
    describe("Loop", () => {
      module V = Verify.Loop(Int.Multiplicative.Loop);
      property1("should satisfy identity", arb_int', V.identity)
    });
  });

  describe("Subtractive", () => {
    describe("Quasigroup", () => {
      module V = Verify.Quasigroup(Int.Subtractive.Quasigroup);
      property3(
        "should satisfy left cancellative",
        arb_int', arb_int', arb_int',
        V.left_cancellative
      );
      property3(
        "should satisfy right cancellative",
        arb_int', arb_int', arb_int',
        V.right_cancellative
      );
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

  describe("Ring", () => {
    module V = Verify.Ring(Int.Ring);
    property1("should satisfy additive inverse", arb_int', V.additive_inverse);
  });

  describe("Commutative Ring", () => {
    module V = Verify.Commutative_Ring(Int.Commutative_Ring);
    property2(
      "should satisfy multiplicative commutativity",
      arb_int', arb_int',
      V.multiplicative_commutativity
    );
  });

  describe("Euclidean Ring", () => {
    module V = Verify.Euclidean_Ring(Int.Euclidean_Ring);
    it("should be a non zero ring (zero is not one)", () => {
      expect(V.non_zero_ring).to_be(true);
    });
    property2(
      "should satisfy integral domain", arb_int', arb_int', V.integral_domain
    );
    property1("should satisfy non negative degree", arb_int', V.non_negative_degree);
    property2(
      "should satisfy the properties for remainder", arb_int', arb_int', V.remainder
    );
    property2(
      "should satisfy submultiplicative", arb_int', arb_int', V.submultiplicative
    );
  });
});
