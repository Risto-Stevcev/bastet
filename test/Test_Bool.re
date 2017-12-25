open Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
module Fn = Infix.Semigroupoid(Function.Semigroupoid);


describe("Bool", () => Fn.({
  describe("Conjunctive", () => {
    describe("Semigroup", () => {
      module V = Verify.Semigroup(Bool.Conjunctive.Semigroup);
      property3(
        "should satisfy associativity", arb_bool, arb_bool, arb_bool, (a, b, c) => {
          let (a', b', c') = (a |> Js.to_bool, b |> Js.to_bool, c |> Js.to_bool);
          V.associativity(a', b', c')
        }
      );
    });

    describe("Monoid", () => {
      module V = Verify.Monoid(Bool.Conjunctive.Monoid);
      property1("should satisfy neutrality", arb_bool, V.neutral << Js.to_bool)
    });
  });

  describe("Disjunctive", () => {
    describe("Semigroup", () => {
      module V = Verify.Semigroup(Bool.Disjunctive.Semigroup);

      property3(
        "should satisfy associativity", arb_bool, arb_bool, arb_bool, (a, b, c) => {
        let (a', b', c') = (a |> Js.to_bool, b |> Js.to_bool, c |> Js.to_bool);
        V.associativity(a', b', c')
      })
    });

    describe("Monoid", () => {
      module V = Verify.Monoid(Bool.Disjunctive.Monoid);
      property1("should satisfy neutrality", arb_bool, V.neutral << Js.to_bool)
    });
  });

  describe("Eq", () => {
    module V = Verify.Eq(Bool.Eq);
    property1("should satisfy reflexivity", arb_bool, V.reflexivity << Js.to_bool);
    property2("should satisfy symmetry", arb_bool, arb_bool, (a, b) => {
      let (a', b') = (Js.to_bool(a), Js.to_bool(b));
      V.symmetry(a', b');
    });
    property3("should satisfy transitivity", arb_bool, arb_bool, arb_bool, (a, b, c) => {
      let (a', b', c') = (Js.to_bool(a), Js.to_bool(b), Js.to_bool(c));
      V.transitivity(a', b', c');
    })
  });
}));
