open BsMochajs.Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
let ((<.)) = Function.Infix.((<.));

module ComparePromise = {
  type t('a) = Js.Promise.t('a);
  let eq = (a, b) =>
    Js.Promise.then_(a' => {
      Js.Promise.then_(b' => Js.Promise.resolve(a' == b'), b)
    }, a)
    |> Obj.magic;
};

describe("Promise", () => {
  let promise = a =>
    Js.Promise.make((~resolve, ~reject as _) => {
      Js.Global.setTimeout(() => [@bs] resolve(a), 10) |> ignore;
    });

  describe("Functor", () => {
    module V = Verify.Compare.Functor(Promise.Functor, ComparePromise);
    async_property1(
      "should satisfy identity", arb_nat, Obj.magic <. V.identity <. promise
    );
    async_property1(
      "should satisfy composition",
      arb_nat,
      Obj.magic <. V.composition((++)("!"), string_of_int) <. promise
    );
  });

  describe("Apply", () => {
    module V = Verify.Compare.Apply(Promise.Apply, ComparePromise);
    async_property1(
      "should satisfy associative composition",
      arb_nat,
      Obj.magic
        <. V.associative_composition(
            Js.Promise.resolve((++)("!")), Js.Promise.resolve(string_of_int)
           )
        <. promise
    );
  });

  describe("Applicative", () => {
    module V = Verify.Compare.Applicative(Promise.Applicative, ComparePromise);
    async_property1(
      "should satisfy identity", arb_nat, Obj.magic <. V.identity <. promise
    );
    async_property1(
      "should satisfy homomorphism", arb_nat, Obj.magic <. V.homomorphism(string_of_int)
    );
  });

  describe("Monad", () => {
    module V = Verify.Compare.Monad(Promise.Monad, ComparePromise);
    let pure = Promise.Applicative.pure;
    async_property1(
      "should satisfy associativity",
      arb_nat,
      Obj.magic <. V.associativity(pure <. string_of_int, pure <. (++)("!")) <. promise
    );
    async_property1(
      "should satisfy identity", arb_nat, Obj.magic <. V.identity(pure <. string_of_int)
    );
  });
});
