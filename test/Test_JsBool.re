open BsMocha.Mocha;
open BsChai.Expect.Expect;
open BsChai.Expect.Combos.End;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
let (<.) = Function.Infix.(<.);


describe("JsBool", () => JsBool.Infix.({
  describe("Example Usage", () => {
    it("should `and` two values together", () => {
      expect(Js.true_ <&&> Js.true_) |> to_be(Js.true_);
      expect(Js.true_ <&&> Js.false_) |> to_be(Js.false_);
      expect(Js.false_ <&&> Js.true_) |> to_be(Js.false_);
      expect(Js.false_ <&&> Js.false_) |> to_be(Js.false_);
    });
    it("should `or` two values together", () => {
      expect(Js.true_ <||> Js.true_) |> to_be(Js.true_);
      expect(Js.true_ <||> Js.false_) |> to_be(Js.true_);
      expect(Js.false_ <||> Js.true_) |> to_be(Js.true_);
      expect(Js.false_ <||> Js.false_) |> to_be(Js.false_);
    });
    it("should `not` a value", () => {
      let not = JsBool.Heyting_Algebra.not;
      expect(not(Js.true_)) |> to_be(Js.false_);
      expect(not(Js.false_)) |> to_be(Js.true_);
    });
    it("should `implies` a value", () => {
      expect(Js.true_ --> Js.true_) |> to_be(Js.true_);
      expect(Js.true_ --> Js.false_) |> to_be(Js.false_);
      expect(Js.false_ --> Js.true_) |> to_be(Js.true_);
      expect(Js.false_ --> Js.false_) |> to_be(Js.true_);
    })
  });
  describe("Conjunctive", () => {
    describe("Medial Magma", () => {
      module V = Verify.Medial_Magma(JsBool.Conjunctive.Medial_Magma);
      property4(
        "should satisfy bicommutativity",
        arb_js_bool, arb_js_bool, arb_js_bool, arb_js_bool,
        V.bicommutativity
      )
    });
    describe("Semigroup", () => {
      module V = Verify.Semigroup(JsBool.Conjunctive.Semigroup);
      property3(
        "should satisfy associativity", arb_js_bool, arb_js_bool, arb_js_bool, V.associativity
      );
    });
    describe("Monoid", () => {
      module V = Verify.Monoid(JsBool.Conjunctive.Monoid);
      property1("should satisfy identity", arb_js_bool, V.identity)
    });
  });

  describe("Disjunctive", () => {
    describe("Medial Magma", () => {
      module V = Verify.Medial_Magma(JsBool.Disjunctive.Medial_Magma);
      property4(
        "should satisfy bicommutativity",
        arb_js_bool, arb_js_bool, arb_js_bool, arb_js_bool,
        V.bicommutativity
      )
    });
    describe("Semigroup", () => {
      module V = Verify.Semigroup(JsBool.Disjunctive.Semigroup);
      property3(
        "should satisfy associativity", arb_js_bool, arb_js_bool, arb_js_bool, V.associativity
      )
    });
    describe("Monoid", () => {
      module V = Verify.Monoid(JsBool.Disjunctive.Monoid);
      property1("should satisfy identity", arb_js_bool, V.identity)
    });
  });

  describe("Eq", () => {
    module V = Verify.Eq(JsBool.Eq);
    property1("should satisfy reflexivity", arb_js_bool, V.reflexivity);
    property2("should satisfy symmetry", arb_js_bool, arb_js_bool, V.symmetry);
    property3(
      "should satisfy transitivity", arb_js_bool, arb_js_bool, arb_js_bool, V.transitivity
    )
  });

  describe("Ord", () => {
    module V = Verify.Ord(JsBool.Ord);
    property1("should satisfy reflexivity", arb_js_bool, V.reflexivity);
    property2("should satisfy antisymmetry", arb_js_bool, arb_js_bool, V.antisymmetry);
    property3(
      "should satisfy transitivity", arb_js_bool, arb_js_bool, arb_js_bool, V.transitivity
    )
  });

  describe("Join_Semilattice", () => {
    module V = Verify.Join_Semilattice(JsBool.Join_Semilattice);
    property3(
      "should satisfy associativity", arb_js_bool, arb_js_bool, arb_js_bool, V.associativity
    );
    property2("should satisfy commutativity", arb_js_bool, arb_js_bool, V.commutativity);
    property1("should satisfy idempotency", arb_js_bool, V.idempotency);
  });

  describe("Meet_Semilattice", () => {
    module V = Verify.Meet_Semilattice(JsBool.Meet_Semilattice);
    property3(
      "should satisfy associativity", arb_js_bool, arb_js_bool, arb_js_bool, V.associativity
    );
    property2("should satisfy commutativity", arb_js_bool, arb_js_bool, V.commutativity);
    property1("should satisfy idempotency", arb_js_bool, V.idempotency);
  });

  describe("Bounded_Join_Semilattice", () => {
    module V = Verify.Bounded_Join_Semilattice(JsBool.Bounded_Join_Semilattice);
    property1("should satisfy identity", arb_js_bool, V.identity);
  });

  describe("Bounded_Meet_Semilattice", () => {
    module V = Verify.Bounded_Meet_Semilattice(JsBool.Bounded_Meet_Semilattice);
    property1("should satisfy identity", arb_js_bool, V.identity);
  });

  describe("Lattice", () => {
    module V = Verify.Lattice(JsBool.Lattice);
    property2("should satisfy absorption", arb_js_bool, arb_js_bool, V.absorption);
  });

  describe("Bounded_Lattice", () => {
    module V = Verify.Bounded_Lattice(JsBool.Bounded_Lattice);
    property2("should satisfy absorption", arb_js_bool, arb_js_bool, V.absorption);
  });

  describe("Distributive_Lattice", () => {
    module V = Verify.Distributive_Lattice(JsBool.Distributive_Lattice);
    property3(
      "should satisfy distributivity", arb_js_bool, arb_js_bool, arb_js_bool, V.distributivity
    );
  });

  describe("Bounded_Distributive_Lattice", () => {
    module V = Verify.Bounded_Distributive_Lattice(JsBool.Bounded_Distributive_Lattice);
    property3(
      "should satisfy distributivity", arb_js_bool, arb_js_bool, arb_js_bool, V.distributivity
    );
  });

  describe("Heyting_Algebra", () => {
    module V = Verify.Heyting_Algebra(JsBool.Heyting_Algebra);
    property1("should satisfy pseudocomplement", arb_js_bool, V.pseudocomplement);
    property3(
      "should satisfy relative pseudocomplement",
      arb_js_bool, arb_js_bool, arb_js_bool,
      V.relative_pseudocomplement
    );
  });

  describe("Involutive_Heyting_Algebra", () => {
    module V = Verify.Involutive_Heyting_Algebra(JsBool.Involutive_Heyting_Algebra);
    property1("should satisfy involution", arb_js_bool, V.involution);
  });

  describe("Boolean_Algebra", () => {
    module V = Verify.Boolean_Algebra(JsBool.Boolean_Algebra);
    property1("should satisfy the law of excluded middle", arb_js_bool, V.excluded_middle);
  });
}));
