open BsMocha.Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
let (<.) = Function.Infix.(<.);

describe("Bool", () => {
  describe("Conjunctive", () => {
    describe("Medial Magma", () => {
      module V = Verify.Medial_Magma(Bool.Conjunctive.Medial_Magma);
      property4(
        "should satisfy bicommutativity",
        arb_bool,
        arb_bool,
        arb_bool,
        arb_bool,
        V.bicommutativity,
      );
    });
    describe("Semigroup", () => {
      module V = Verify.Semigroup(Bool.Conjunctive.Semigroup);
      property3(
        "should satisfy associativity",
        arb_bool,
        arb_bool,
        arb_bool,
        V.associativity,
      );
    });
    describe("Monoid", () => {
      module V = Verify.Monoid(Bool.Conjunctive.Monoid);
      property1("should satisfy identity", arb_bool, V.identity);
    });
  });

  describe("Disjunctive", () => {
    describe("Medial Magma", () => {
      module V = Verify.Medial_Magma(Bool.Disjunctive.Medial_Magma);
      property4(
        "should satisfy bicommutativity",
        arb_bool,
        arb_bool,
        arb_bool,
        arb_bool,
        V.bicommutativity,
      );
    });
    describe("Semigroup", () => {
      module V = Verify.Semigroup(Bool.Disjunctive.Semigroup);
      property3(
        "should satisfy associativity",
        arb_bool,
        arb_bool,
        arb_bool,
        V.associativity,
      );
    });
    describe("Monoid", () => {
      module V = Verify.Monoid(Bool.Disjunctive.Monoid);
      property1("should satisfy identity", arb_bool, V.identity);
    });
  });

  describe("Eq", () => {
    module V = Verify.Eq(Bool.Eq);
    property1("should satisfy reflexivity", arb_bool, V.reflexivity);
    property2("should satisfy symmetry", arb_bool, arb_bool, V.symmetry);
    property3(
      "should satisfy transitivity",
      arb_bool,
      arb_bool,
      arb_bool,
      V.transitivity,
    );
  });

  describe("Ord", () => {
    module V = Verify.Ord(Bool.Ord);
    property1("should satisfy reflexivity", arb_bool, V.reflexivity);
    property2(
      "should satisfy antisymmetry",
      arb_bool,
      arb_bool,
      V.antisymmetry,
    );
    property3(
      "should satisfy transitivity",
      arb_bool,
      arb_bool,
      arb_bool,
      V.transitivity,
    );
  });

  describe("Join_Semilattice", () => {
    module V = Verify.Join_Semilattice(Bool.Join_Semilattice);
    property3(
      "should satisfy associativity",
      arb_bool,
      arb_bool,
      arb_bool,
      V.associativity,
    );
    property2(
      "should satisfy commutativity",
      arb_bool,
      arb_bool,
      V.commutativity,
    );
    property1("should satisfy idempotency", arb_bool, V.idempotency);
  });

  describe("Meet_Semilattice", () => {
    module V = Verify.Meet_Semilattice(Bool.Meet_Semilattice);
    property3(
      "should satisfy associativity",
      arb_bool,
      arb_bool,
      arb_bool,
      V.associativity,
    );
    property2(
      "should satisfy commutativity",
      arb_bool,
      arb_bool,
      V.commutativity,
    );
    property1("should satisfy idempotency", arb_bool, V.idempotency);
  });

  describe("Bounded_Join_Semilattice", () => {
    module V = Verify.Bounded_Join_Semilattice(Bool.Bounded_Join_Semilattice);
    property1("should satisfy identity", arb_bool, V.identity);
  });

  describe("Bounded_Meet_Semilattice", () => {
    module V = Verify.Bounded_Meet_Semilattice(Bool.Bounded_Meet_Semilattice);
    property1("should satisfy identity", arb_bool, V.identity);
  });

  describe("Lattice", () => {
    module V = Verify.Lattice(Bool.Lattice);
    property2("should satisfy absorption", arb_bool, arb_bool, V.absorption);
  });

  describe("Bounded_Lattice", () => {
    module V = Verify.Bounded_Lattice(Bool.Bounded_Lattice);
    property2("should satisfy absorption", arb_bool, arb_bool, V.absorption);
  });

  describe("Distributive_Lattice", () => {
    module V = Verify.Distributive_Lattice(Bool.Distributive_Lattice);
    property3(
      "should satisfy distributivity",
      arb_bool,
      arb_bool,
      arb_bool,
      V.distributivity,
    );
  });

  describe("Bounded_Distributive_Lattice", () => {
    module V =
      Verify.Bounded_Distributive_Lattice(Bool.Bounded_Distributive_Lattice);
    property3(
      "should satisfy distributivity",
      arb_bool,
      arb_bool,
      arb_bool,
      V.distributivity,
    );
  });

  describe("Heyting_Algebra", () => {
    module V = Verify.Heyting_Algebra(Bool.Heyting_Algebra);
    property1(
      "should satisfy pseudocomplement",
      arb_bool,
      V.pseudocomplement,
    );
    property3(
      "should satisfy relative pseudocomplement",
      arb_bool,
      arb_bool,
      arb_bool,
      V.relative_pseudocomplement,
    );
  });

  describe("Involutive_Heyting_Algebra", () => {
    module V =
      Verify.Involutive_Heyting_Algebra(Bool.Involutive_Heyting_Algebra);
    property1("should satisfy involution", arb_bool, V.involution);
  });

  describe("Boolean_Algebra", () => {
    module V = Verify.Boolean_Algebra(Bool.Boolean_Algebra);
    property1(
      "should satisfy the law of excluded middle",
      arb_bool,
      V.excluded_middle,
    );
  });
});
