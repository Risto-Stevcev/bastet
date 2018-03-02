open BsMocha.Mocha;
open BsChai.Expect.Expect;
open BsChai.Expect.Combos.End;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;


describe("Float", () => {
  /* Intentionally restricted range to avoid arithmetic overflows as much as possible */
  let arb_float' =
    arb_float(Float.Bounded.bottom /. 100000.0, Float.Bounded.top /. 100000.0);
  let tolerance = 0.01;

  module CompareFloat = {
    type t = float;
    let eq = Float.approximately_equal(~tolerance);
  };

  describe("Additive", () => {
    describe("Medial Magma", () => {
      module V = Verify.Medial_Magma(Float.Additive.Medial_Magma);
      property4(
        "should satisfy bicommutativity",
        arb_float', arb_float', arb_float', arb_float',
        V.bicommutativity
      )
    });
    describe("Semigroup", () => {
      module V = Verify.Semigroup(Float.Additive.Semigroup);
      property3(
        "should satisfy associativity",
        arb_float', arb_float', arb_float',
        V.associativity
      )
    });
    describe("Monoid", () => {
      module V = Verify.Monoid(Float.Additive.Monoid);
      property1("should satisfy identity", arb_float', V.identity)
    });
    describe("Quasigroup", () => {
      module V = Verify.Quasigroup(Float.Additive.Quasigroup);
      property3(
        "should satisfy cancellative",
        arb_float', arb_float', arb_float',
        V.cancellative
      );
    });
    describe("Loop", () => {
      module V = Verify.Loop(Float.Additive.Loop);
      property1("should satisfy identity", arb_float', V.identity)
    });
    describe("Group", () => {
      module V = Verify.Group(Float.Additive.Group);
      property1("should satisfy invertibility", arb_float', V.invertibility);
      property3(
        "should satisfy associativity",
        arb_float', arb_float', arb_float',
        V.associativity
      )
    });
    describe("Abelian Group", () => {
      module V = Verify.Abelian_Group(Float.Additive.Abelian_Group);
      property2(
        "should satisfy commutativity", arb_float', arb_float', V.commutativity
      )
    });
  });

  describe("Multiplicative", () => {
    describe("Medial Magma", () => {
      module V =
        Verify.Compare.Medial_Magma(Float.Multiplicative.Medial_Magma, CompareFloat);
      let arb_float'' = arb_float(1000.0, -1000.0); /* to avoid arithmetic overflows */
      property4(
        "should satisfy bicommutativity",
        arb_float'', arb_float'', arb_float'', arb_float'',
        V.bicommutativity
      )
    });

    describe("Semigroup", () => {
      module V = Verify.Semigroup(Float.Multiplicative.Semigroup);
      property3(
        "should satisfy associativity", arb_float', arb_float', arb_float', V.associativity
      )
    });
    describe("Monoid", () => {
      module V = Verify.Monoid(Float.Multiplicative.Monoid);
      property1("should satisfy identity", arb_float', V.identity)
    });
    describe("Quasigroup", () => {
      module V = Verify.Quasigroup(Float.Multiplicative.Quasigroup);
      property3(
        "should satisfy cancellative", arb_float', arb_float', arb_float', V.cancellative
      );
    });
    describe("Loop", () => {
      module V = Verify.Loop(Float.Multiplicative.Loop);
      property1("should satisfy identity", arb_float', V.identity)
    });
  });

  describe("Subtractive", () => {
    describe("Medial Magma", () => {
      module V = Verify.Medial_Magma(Float.Subtractive.Medial_Magma);
      property4(
        "should satisfy bicommutativity",
        arb_float', arb_float', arb_float', arb_float',
        V.bicommutativity
      )
    });
    describe("Quasigroup", () => {
      module V = Verify.Quasigroup(Float.Subtractive.Quasigroup);
      property3(
        "should satisfy cancellative", arb_float', arb_float', arb_float', V.cancellative
      );
    });
  });

  describe("Divisive", () => {
    describe("Medial Magma", () => {
      module V = Verify.Compare.Medial_Magma(Float.Divisive.Medial_Magma, CompareFloat);
      property4(
        "should satisfy bicommutativity",
        arb_float', arb_float', arb_float', arb_float',
        V.bicommutativity
      )
    });
    describe("Quasigroup", () => {
      module V = Verify.Quasigroup(Float.Divisive.Quasigroup);
      property3(
        "should satisfy cancellative", arb_float', arb_float', arb_float', V.cancellative
      );
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
    module V = Verify.Compare.Semiring(Float.Semiring, CompareFloat);
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
      V.multiplicative_associativity
    );
    property1(
      "should satisfy multiplicative identity", arb_float', V.multiplicative_identity
    );
    property3(
      "should satisfy distributivity", arb_float', arb_float', arb_float', V.distributivity
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
    module V = Verify.Compare.Division_Ring(Float.Division_Ring, CompareFloat);
    it("should be a non-zero ring (one is not zero)", () => {
      expect(V.non_zero_ring) |> to_be(true);
    });
    property1(
      "should satisfy multiplicative inverse", arb_float', V.multiplicative_inverse
    );
  });

  describe("Euclidean Ring", () => {
    module V = Verify.Compare.Euclidean_Ring(Float.Euclidean_Ring, CompareFloat);
    it("should be a non zero ring (zero is not one)", () => {
      expect(V.non_zero_ring) |> to_be(true);
    });
    property2(
      "should satisfy integral domain", arb_float', arb_float', V.integral_domain
    );
    property1("should satisfy non negative degree", arb_float', V.non_negative_degree);
    property2(
      "should satisfy the properties for remainder", arb_float', arb_float', V.remainder
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
