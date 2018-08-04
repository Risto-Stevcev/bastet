open BsMocha.Mocha;
open BsChai.Expect.Expect;
open BsChai.Expect.Combos.End;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
open Functors;
let (<.) = Function.Infix.(<.);

describe("Tuple", () => {
  describe("Semigroup", () => {
    module S = Tuple.Semigroup(String.Semigroup, Int.Additive.Semigroup);
    module V = Verify.Semigroup(S);
    property3(
      "should satisfy associativity",
      arb_tuple((arb_string, arb_nat)),
      arb_tuple((arb_string, arb_nat)),
      arb_tuple((arb_string, arb_nat)),
      V.associativity,
    );
  });

  describe("Monoid", () => {
    module M = Tuple.Monoid(String.Monoid, Int.Additive.Monoid);
    module V = Verify.Monoid(M);
    property1(
      "should satisfy identity",
      arb_tuple((arb_string, arb_nat)),
      V.identity,
    );
  });

  describe("Functor", () => {
    module V = Verify.Functor(TupleF.String.Functor);
    property1(
      "should satisfy identity",
      arb_tuple((arb_string, arb_nat)),
      V.identity,
    );
    property1(
      "should satisfy composition", arb_tuple((arb_string, arb_nat)), a =>
      V.composition((++)("!"), string_of_int, a)
    );
  });

  describe("Apply", () => {
    module V = Verify.Apply(TupleF.String.Apply);
    property1(
      "should satisfy associative composition",
      arb_tuple((arb_string, arb_nat)),
      n =>
      V.associative_composition(
        (String.Monoid.empty, (++)("!")),
        (String.Monoid.empty, string_of_int),
        n,
      )
    );
  });

  describe("Applicative", () => {
    module V = Verify.Applicative(TupleF.String.Applicative);
    property1(
      "should satisfy identity",
      arb_tuple((arb_string, arb_nat)),
      V.identity,
    );
    property1(
      "should satisfy homomorphism",
      arb_tuple((arb_string, arb_nat)),
      V.homomorphism(TupleF.String.Functor.map(string_of_int)),
    );
    property1(
      "should satisfy interchange",
      arb_nat,
      V.interchange((String.Monoid.empty, string_of_int)),
    );
  });

  describe("Monad", () => {
    module V = Verify.Monad(TupleF.String.Monad);
    let pure = TupleF.String.Applicative.pure;
    property1(
      "should satisfy associativity",
      arb_tuple((arb_string, arb_nat)),
      V.associativity(pure <. string_of_int, pure <. (++)("!")),
    );
    property1(
      "should satisfy identity",
      arb_nat,
      V.identity(pure <. string_of_int),
    );
  });

  describe("Foldable", () => {
    open TupleF.String.Foldable;
    let empty = String.Monoid.empty;
    it("should do a left fold", () => {
      expect(fold_left((+), 0, (empty, 123))) |> to_be(123);
      expect(fold_left((-), 10, (empty, 321))) |> to_be(-311);
    });

    it("should do a right fold", () =>
      expect(fold_right((-), 10, (empty, 321))) |> to_be(311)
    );

    it("should do a map fold (int)", () => {
      module F = Fold_Map(Int.Additive.Monoid);
      expect(F.fold_map(Function.Category.id, (empty, 123))) |> to_be(123);
    });

    it("should do a map fold (list)", () => {
      module F = Fold_Map_Plus(List.Plus);
      expect(F.fold_map(List.Applicative.pure, (empty, [1, 2, 3])))
      |> to_be([[1, 2, 3]]);
    });
  });

  describe("Traversable", () => {
    let (traverse, sequence) =
      TupleF.String.List.Traversable.(traverse, sequence);
    it("should traverse the array", () => {
      let positive_int = x => x >= 0 ? [x] : [];
      expect(traverse(positive_int, ("foo", 123)))
      |> to_be([("foo", 123)]);
      expect(traverse(positive_int, ("bar", (-123)))) |> to_be([]);
    });
    it("should sequence the array", () => {
      expect(sequence(("foo", [123]))) |> to_be([("foo", 123)]);
      expect(sequence(("bar", []))) |> to_be([]);
    });
  });

  describe("Eq", () => {
    module E = Tuple.Eq(String.Eq, Int.Eq);
    module V = Verify.Eq(E);
    property1(
      "should satisfy reflexivity",
      arb_tuple((arb_string, arb_nat)),
      V.reflexivity,
    );
    property2(
      "should satisfy symmetry",
      arb_tuple((arb_string, arb_nat)),
      arb_tuple((arb_string, arb_nat)),
      V.symmetry,
    );
    property3(
      "should satisfy transitivity",
      arb_tuple((arb_string, arb_nat)),
      arb_tuple((arb_string, arb_nat)),
      arb_tuple((arb_string, arb_nat)),
      V.transitivity,
    );
  });

  describe("Bifunctor", () => {
    module V = Verify.Bifunctor(Tuple.Bifunctor);
    property1(
      "should satisfy identity",
      arb_tuple((arb_string, arb_nat)),
      V.identity,
    );
    property1(
      "should satisfy composition",
      arb_tuple((arb_string, arb_nat)),
      V.composition((++)("!"), ( *. )(3.0), (++)("-"), float_of_int),
    );
  });
});
