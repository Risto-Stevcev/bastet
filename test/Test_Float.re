open Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;


describe("Float", () => {
  let arb_float' = arb_float(-10000.0, 10000.0);
  let tolerance = 0.001;

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

  describe("Semiring", () => {
    module V = Verify.Semiring(Float.Semiring);
    let ((|+|), (|*|)) = Float.Infix.((|+|), (|*|));

    property3(
      "should satisfy additive associativity",
      arb_float', arb_float', arb_float',
      V.additive_associativity
    );
    property1("should satisfy additive identity", arb_float', V.additive_identity);
    property2("should satisfy commutativity", arb_float', arb_float', V.commutativity);
    property3(
      "should satisfy multiplicative associativity",
      arb_float', arb_float', arb_float',
      (a, b, c) => Float.approximately_equal(
        ~tolerance,
        (a |*| b) |*| c,
        a |*| (b |*| c)
      )
    );
    property1(
      "should satisfy multiplicative identity", arb_float', V.multiplicative_identity
    );
    property3(
      "should satisfy left distributivity",
      arb_float', arb_float', arb_float',
      (a, b, c) => Float.approximately_equal(
        ~tolerance,
        a |*| (b |+| c),
        (a |*| b) |+| (a |*| c)
      )
    );
    property3(
      "should satisfy right distributivity",
      arb_float', arb_float', arb_float',
      (a, b, c) => Float.approximately_equal(
        ~tolerance,
        (a |+| b) |*| c,
        (a |*| c) |+| (b |*| c)
      )
    );
  });

  describe("Ring", () => {
    module V = Verify.Ring(Float.Ring);
    property1("should satisfy additive inverse", arb_float', V.additive_inverse);
  });

  describe("Commutative Ring", () => {
    module V = Verify.Commutative_Ring(Float.Commutative_Ring);
    property2(
      "should satisfy multiplicative commutativity",
      arb_float', arb_float',
      V.multiplicative_commutativity
    );
  });

  describe("Division Ring", () => {
    let (|*|) = Float.Infix.((|*|));
    module V = Verify.Division_Ring(Float.Division_Ring);
    it("should be a non-zero ring (one is not zero)", () => {
      expect(V.non_zero_ring).to_be(true);
    });
    property1(
      "should satisfy multiplicative inverse", arb_float',
      (a) => Float.approximately_equal(
        ~tolerance,
        Float.Division_Ring.reciprocal(a) |*| a,
        Float.Semiring.one
      )
    );
  });

  describe("Euclidean Ring", () => {
    module V = Verify.Euclidean_Ring(Float.Euclidean_Ring);
    let (zero, degree) = Float.Euclidean_Ring.((zero, degree));

    it("should be a non zero ring (zero is not one)", () => {
      expect(V.non_zero_ring).to_be(true);
    });
    property2(
      "should satisfy integral domain", arb_float', arb_float', V.integral_domain
    );
    property1("should satisfy non negative degree", arb_float', V.non_negative_degree);
    property2(
      "should satisfy the properties for remainder",
      arb_float', arb_float',
      (a, b) => Float.Infix.({
        if (b != zero) {
          let q = a |/| b;
          let r = a |%| b;
          Float.approximately_equal(~tolerance, a, q |*| b |+| r) &&
          (r == zero || (degree(r) < degree(b)))
        }
        else { true }
      })
    );
    property2(
      "should satisfy submultiplicative", arb_float', arb_float', V.submultiplicative
    );
  });

  describe("Field", () => {
    module V = Verify.Field(Float.Field);
    property2(
      "should satisfy non zero multiplicative inverse",
      arb_float', arb_float',
      V.non_zero_multiplicative_inverse
    );
  });
});
