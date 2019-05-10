open BsMocha.Mocha;
open BsChai.Expect.Expect;
open BsChai.Expect.Combos.End;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
let ((<.), (>.)) = Function.Infix.((<.), (>.));

describe("Function", () => {
  describe("Functor", () => {
    property1(
      "should satisfy identity",
      arb_nat,
      n => {
        module V =
          Verify.Compare.Functor(
            (
              Function.Functor({
                type t = int;
              })
            ),
            {
              type t('a) = int => 'a;
              let eq = Obj.magic((a, b) => a(n) == b(n));
            },
          );
        V.identity(string_of_int);
      },
    );

    property1(
      "should satisfy composition",
      arb_nat,
      n => {
        module V =
          Verify.Compare.Functor(
            (
              Function.Functor({
                type t = int;
              })
            ),
            {
              type t('a) = int => 'a;
              let eq = Obj.magic((a, b) => a(n) == b(n));
            },
          );
        V.composition((++)("!"), string_of_int, (+)(1));
      },
    );
  });

  describe("Apply", () => {
    property1(
      "should satisfy associative composition",
      arb_nat,
      n => {
        module V =
          Verify.Compare.Apply(
            (
              Function.Apply({
                type t = int;
              })
            ),
            {
              type t('a) = int => 'a;
              let eq = Obj.magic((a, b) => a(n) == b(n));
            },
          );
        V.associative_composition((+), (-), (+)(1));
      },
    );

    it("should compose two infix functions (x - x * y)", () => {
      module Apply_Fn_Int =
        Function.Apply({
          type t = int;
        });
      module Apply_Util = Functions.Apply(Apply_Fn_Int);

      let fn = Apply_Util.lift2(Function.Semigroupoid.compose, (-), ( * ));
      expect(fn(3, 4)) |> to_be(-9);
    });
  });

  describe("Semigroupoid", () =>
    property1(
      "should satisfy associativity",
      arb_nat,
      n => {
        module V =
          Verify.Compare.Semigroupoid(
            Function.Semigroupoid,
            {
              type t('a, 'b) = 'a => 'b;
              let eq = Obj.magic((a, b) => a(n) == b(n));
            },
          );
        V.associativity((==)("123!"), (++)("!"), string_of_int);
      },
    )
  );

  describe("Category", () =>
    property1(
      "should satisfy identity",
      arb_nat,
      n => {
        module V =
          Verify.Compare.Category(
            Function.Category,
            {
              type t('a, 'b) = 'a => 'b;
              let eq = Obj.magic((a, b) => a(n) == b(n));
            },
          );
        V.identity(string_of_int);
      },
    )
  );

  describe("Profunctor", () => {
    property1(
      "should satisfy identity",
      arb_nat,
      n => {
        module V =
          Verify.Compare.Profunctor(
            Function.Profunctor,
            {
              type t('a, 'b) = 'a => 'b;
              let eq = Obj.magic((a, b) => a(n) == b(n));
            },
          );
        V.identity(string_of_int);
      },
    );
    property1(
      "should satisfy composition",
      arb_nat,
      n => {
        module V =
          Verify.Compare.Profunctor(
            Function.Profunctor,
            {
              type t('a, 'b) = 'a => 'b;
              let eq = Obj.magic((a, b) => a(n) == b(n));
            },
          );
        V.composition(
          float_of_int,
          Js.Float.toString,
          ( * )(4),
          (++)("!"),
          ( *. )(2.0),
        );
      },
    );
  });

  describe("Contravariant", () => {
    property1(
      "should satisfy identity",
      arb_nat,
      n => {
        module V =
          Verify.Compare.Contravariant(
            (
              Function.Contravariant({
                type t = string;
              })
            ),
            {
              type t('a) = 'a => string;
              let eq = Obj.magic((a, b) => a(n) == b(n));
            },
          );
        V.identity(string_of_int);
      },
    );

    property1(
      "should satisfy composition",
      arb_nat,
      n => {
        module V =
          Verify.Compare.Contravariant(
            (
              Function.Contravariant({
                type t = string;
              })
            ),
            {
              type t('a) = 'a => string;
              let eq = Obj.magic((a, b) => a(n) == b(n));
            },
          );
        V.composition((+)(1), string_of_int, (++)("!"));
      },
    );
  });

  describe("Bicontravariant", () => {
    property2(
      "should satisfy identity",
      arb_nat,
      arb_nat,
      (n1, n2) => {
        module V =
          Verify.Compare.Bicontravariant(
            (
              Function.Bicontravariant({
                type t = int;
              })
            ),
            {
              type t('a, 'b) = ('a, 'b) => int;
              let eq = Obj.magic((a, b) => a(n1, n2) == b(n1, n2));
            },
          );
        V.identity((+));
      },
    );

    property2(
      "should satisfy composition",
      arb_nat,
      arb_nat,
      (n1, n2) => {
        module V =
          Verify.Compare.Bicontravariant(
            (
              Function.Bicontravariant({
                type t = int;
              })
            ),
            {
              type t('a, 'b) = ('a, 'b) => int;
              let eq = Obj.magic((a, b) => a(n1, n2) == b(n1, n2));
            },
          );
        V.composition((+)(1), (+)(2), ( * )(3), ( * )(4), (+));
      },
    );
  });
});
