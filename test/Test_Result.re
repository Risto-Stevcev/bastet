open BsMocha.Mocha;
open BsChai.Expect.Expect;
open BsChai.Expect.Combos.End;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
open Js.Result;
type arbitrary('a) = BsJsverify.Verify.Arbitrary.arbitrary('a);
let (<.) = Function.Infix.(<.);
let (const, id) = (Function.const, Function.Category.id);


let arb_result : (arbitrary('a), arbitrary('b)) => arbitrary(Js.Result.t('a, 'b)) =
  (arb_ok, arb_error) => {
    smap(
      e => switch e {
        | Types.Left(l) => Error(l)
        | Types.Right(r) => Ok(r)
      },
      e => switch e {
        | Ok(r) => Types.Right(r)
        | Error(l) => Types.Left(l)
      },
      ~newShow = a => switch a {
        | Ok(a') =>
          "Ok("++ (Js.Json.stringifyAny(a') |> Js.Option.getWithDefault("")) ++")"
        | Error(a') =>
          "Error("++ (Js.Json.stringifyAny(a') |> Js.Option.getWithDefault("")) ++")"
      },
      arb_either(arb_error, arb_ok)
    )
  };

describe("Result", () => {
  describe("Medial Magma", () => {
    module Medial_Magma = 
      Result.Medial_Magma({type t = string}, Int.Additive.Medial_Magma);
    module V = Verify.Medial_Magma(Medial_Magma);
    property4(
      "should satisfy bicommutativity",
      arb_result(arb_nat, arb_string),
      arb_result(arb_nat, arb_string),
      arb_result(arb_nat, arb_string),
      arb_result(arb_nat, arb_string),
      V.bicommutativity
    )
  });

  describe("Semigroup", () => {
    module Semigroup = 
      Result.Semigroup({type t = string}, Int.Additive.Semigroup);
    module V = Verify.Semigroup(Semigroup);
    property3(
      "should satisfy associativity",
      arb_result(arb_nat, arb_string),
      arb_result(arb_nat, arb_string),
      arb_result(arb_nat, arb_string),
      V.associativity
    )
  });

  describe("Functor", () => {
    module V = Verify.Functor(Functors.ResultF.String.Functor);
    property1("should satisfy identity", arb_result(arb_nat, arb_string), V.identity);
    property1(
      "should satisfy composition",
      arb_result(arb_nat, arb_string),
      V.composition((++)("!"), string_of_int)
    );
  });

  describe("Bifunctor", () => {
    module V = Verify.Bifunctor(Result.Bifunctor);
    property1(
      "should satisfy identity", arb_result(arb_string, arb_nat), V.identity
    );
    property1(
      "should satisfy composition",
      arb_result(arb_string, arb_nat),
      V.composition((++)("!"), (*.)(3.0), (++)("-"), float_of_int)
    );
  });

  describe("Apply", () => {
    module V = Verify.Apply(Functors.ResultF.String.Apply);
    property1(
      "should satisfy associative composition",
      arb_result(arb_nat, arb_string),
      (n) => V.associative_composition(Ok((++)("!")), Ok(string_of_int), n)
    )
  });

  describe("Applicative", () => {
    module V = Verify.Applicative(Functors.ResultF.String.Applicative);
    property1("should satisfy identity", arb_result(arb_nat, arb_string), V.identity);
    property1(
      "should satisfy homomorphism",
      arb_result(arb_nat, arb_string),
      V.homomorphism(Functors.ResultF.String.Functor.map(string_of_int))
    );
    property1(
      "should satisfy interchange",
      arb_nat,
      V.interchange(Ok(string_of_int))
    );
  });

  describe("Monad", () => {
    module V = Verify.Monad(Functors.ResultF.String.Monad);
    let pure = Functors.ResultF.String.Applicative.pure;
    property1(
      "should satisfy associativity",
      arb_result(arb_nat, arb_string),
      V.associativity(pure <. string_of_int, pure <. (++)("!"))
    );
    property1(
      "should satisfy identity", arb_nat, V.identity(pure <. string_of_int)
    );
  });

  describe("Alt", () => {
    module V = Verify.Alt(Functors.ResultF.String.Alt);
    property3(
      "should satisfy associativity",
      arb_result(arb_nat, arb_string),
      arb_result(arb_nat, arb_string),
      arb_result(arb_nat, arb_string),
      V.associativity
    );
    property2(
      "should satisfy distributivity",
      arb_result(arb_nat, arb_string),
      arb_result(arb_nat, arb_string),
      V.distributivity(string_of_int)
    )
  });

  describe("Extend", () => {
    module V = Verify.Extend(Functors.ResultF.Bool.Extend);
    property1(
      "should satisfy associativity",
      arb_result(arb_nat, arb_bool),
      V.associativity(
        Result.result(string_of_float, const(String.Monoid.empty)),
        Result.result(float_of_int, const(Float.Additive.Monoid.empty))
      )
    );
  });

  describe("Show", () => {
    it("should show the either value", () => {
      expect(Functors.ResultF.Bool.Int.Show.show(Ok(true))) |> to_be("true");
      expect(Functors.ResultF.Bool.Int.Show.show(Error(123))) |> to_be("123");
    })
  });

  describe("Eq", () => {
    it("should compare two either values for equality", () => {
      module E = Result.Eq(Int.Eq, Int.Eq);
      let eq = Functors.ResultF.Float.Int.Eq.eq;
      expect(eq(Error(123), Error(123))) |> to_be(true);
      expect(eq(Error(123), Error(456))) |> to_be(false);
      expect(eq(Ok(12.3), Ok(12.3))) |> to_be(true);
      expect(eq(Ok(12.3), Ok(45.6))) |> to_be(false);
      expect(eq(Error(123), Ok(45.6))) |> to_be(false);
      expect(E.eq(Error(123), Ok(123))) |> to_be(false);
      expect(E.eq(Ok(123), Error(123))) |> to_be(false);
    })
  });

  describe("Ord", () => {
    it("should compare two either values for equality", () => {
      module E = Result.Ord(Int.Ord, Int.Ord);
      let compare = Functors.ResultF.Float.Int.Ord.compare;
      expect(compare(Error(123), Error(123))) |> to_be(`equal_to); 
      expect(compare(Error(123), Error(456))) |> to_be(`less_than);
      expect(compare(Ok(12.3), Ok(12.3))) |> to_be(`equal_to);
      expect(compare(Ok(12.3), Ok(45.6))) |> to_be(`less_than);
      expect(compare(Error(123), Ok(45.6))) |> to_be(`less_than);
      expect(E.compare(Error(123), Ok(123))) |> to_be(`less_than);
      expect(E.compare(Ok(123), Error(123))) |> to_be(`greater_than);
    })
  });

  describe("Bounded", () => {
    let arb_float' = arb_float(Float.Bounded.bottom, Float.Bounded.top);
    module V = Verify.Bounded(Result.Bounded(Int.Bounded, Float.Bounded));
    property1("should satisfy bounded", arb_result(arb_nat, arb_float'), V.bounded);
  });

  describe("Join_Semilattice", () => {
    module V = Verify.Join_Semilattice(
      Result.Many_Valued_Logic.Join_Semilattice(
        Bool.Join_Semilattice, JsBool.Join_Semilattice
      )
    );
    property3(
      "should satisfy associativity",
      arb_result(arb_bool, arb_js_bool),
      arb_result(arb_bool, arb_js_bool),
      arb_result(arb_bool, arb_js_bool),
      V.associativity
    );
    property2(
      "should satisfy commutativity",
      arb_result(arb_bool, arb_js_bool),
      arb_result(arb_bool, arb_js_bool),
      V.commutativity
    );
    property1(
      "should satisfy idempotency", arb_result(arb_bool, arb_js_bool), V.idempotency
    );
  });

  describe("Meet_Semilattice", () => {
    module V = Verify.Meet_Semilattice(
      Result.Many_Valued_Logic.Meet_Semilattice(
        Bool.Meet_Semilattice, JsBool.Meet_Semilattice
      )
    );
    property3(
      "should satisfy associativity",
      arb_result(arb_bool, arb_js_bool),
      arb_result(arb_bool, arb_js_bool),
      arb_result(arb_bool, arb_js_bool),
      V.associativity
    );
    property2(
      "should satisfy commutativity",
      arb_result(arb_bool, arb_js_bool),
      arb_result(arb_bool, arb_js_bool),
      V.commutativity
    );
    property1(
      "should satisfy idempotency", arb_result(arb_bool, arb_js_bool), V.idempotency
    );
  });

  describe("Bounded_Join_Semilattice", () => {
    module V = Verify.Bounded_Join_Semilattice(
      Result.Many_Valued_Logic.Bounded_Join_Semilattice(
        Bool.Bounded_Join_Semilattice, JsBool.Bounded_Join_Semilattice
      )
    );
    property1("should satisfy identity", arb_result(arb_bool, arb_js_bool), V.identity);
  });

  describe("Bounded_Meet_Semilattice", () => {
    module V = Verify.Bounded_Meet_Semilattice(
      Result.Many_Valued_Logic.Bounded_Meet_Semilattice(
        Bool.Bounded_Meet_Semilattice, JsBool.Bounded_Meet_Semilattice
      )
    );
    property1("should satisfy identity", arb_result(arb_bool, arb_js_bool), V.identity);
  });

  describe("Lattice", () => {
    module V = Verify.Lattice(
      Result.Many_Valued_Logic.Lattice(Bool.Lattice, JsBool.Lattice)
    );
    property2(
      "should satisfy absorption",
      arb_result(arb_bool, arb_js_bool),
      arb_result(arb_bool, arb_js_bool),
      V.absorption
    );
  });

  describe("Bounded_Lattice", () => {
    module V = Verify.Bounded_Lattice(
      Result.Many_Valued_Logic.Bounded_Lattice(Bool.Bounded_Lattice, JsBool.Bounded_Lattice)
    );
    property2(
      "should satisfy absorption",
      arb_result(arb_bool, arb_js_bool),
      arb_result(arb_bool, arb_js_bool),
      V.absorption
    );
  });

  describe("Distributive_Lattice", () => {
    module V = Verify.Distributive_Lattice(
      Result.Many_Valued_Logic.Distributive_Lattice(
        Bool.Distributive_Lattice, JsBool.Distributive_Lattice
      )
    );
    property3(
      "should satisfy distributivity",
      arb_result(arb_bool, arb_js_bool),
      arb_result(arb_bool, arb_js_bool),
      arb_result(arb_bool, arb_js_bool),
      V.distributivity
    );
  });

  describe("Bounded_Distributive_Lattice", () => {
    module V = Verify.Bounded_Distributive_Lattice(
      Result.Many_Valued_Logic.Bounded_Distributive_Lattice(
        Bool.Bounded_Distributive_Lattice, JsBool.Bounded_Distributive_Lattice
      )
    );
    property3(
      "should satisfy distributivity",
      arb_result(arb_bool, arb_js_bool),
      arb_result(arb_bool, arb_js_bool),
      arb_result(arb_bool, arb_js_bool),
      V.distributivity
    );
  });

  describe("Heyting_Algebra", () => {
    module V = Verify.Heyting_Algebra(
      Result.Many_Valued_Logic.Heyting_Algebra(
        Bool.Heyting_Algebra, JsBool.Heyting_Algebra
      )
    );
    property1(
      "should satisfy pseudocomplement",
      arb_result(arb_bool, arb_js_bool),
      V.pseudocomplement
    );
    property3(
      "should satisfy relative pseudocomplement",
      arb_result(arb_bool, arb_js_bool),
      arb_result(arb_bool, arb_js_bool),
      arb_result(arb_bool, arb_js_bool),
      V.relative_pseudocomplement
    );
  });

  describe("Involutive_Heyting_Algebra", () => {
    module V = Verify.Involutive_Heyting_Algebra(
      Result.Many_Valued_Logic.Involutive_Heyting_Algebra(
        Bool.Involutive_Heyting_Algebra, JsBool.Involutive_Heyting_Algebra
      )
    );
    property1("should satisfy involution", arb_result(arb_bool, arb_js_bool), V.involution);
  });

  describe("Boolean_Algebra", () => {
    module V = Verify.Compare.Boolean_Algebra(
      Result.Many_Valued_Logic.Boolean_Algebra(
        Bool.Boolean_Algebra, JsBool.Boolean_Algebra
      ),
      Result.Many_Valued_Logic.Quasireflexive_Eq(
        Bool.Boolean_Algebra, JsBool.Boolean_Algebra
      )
    );
    property1(
      "should satisfy the law of excluded middle",
      arb_result(arb_bool, arb_js_bool),
      V.excluded_middle
    );
  });
});
