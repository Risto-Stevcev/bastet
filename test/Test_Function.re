open Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
let (<.) = Function.Infix.(<.);


describe("Function", () => {
  describe("Semigroupoid", () => {
    property1("should satisfy associativity", arb_nat, (n) => {
      let (a, b, c) = ((==)("123!"), (++)("!"), string_of_int);
      (a <. b <. c)(n) == (a <. (b <. c))(n)
    });
  });

  describe("Category", () => {
    property1("should satisfy identity", arb_nat, (n) => {
      (Function.Category.id <. string_of_int)(n) == string_of_int(n) &&
      (string_of_int <. Function.Category.id)(n) == string_of_int(n)
    });
  });
});
