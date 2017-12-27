open Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
let ((<.), (>.)) = Function.Infix.((<.), (>.));


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

  describe("Profunctor", () => {
    let id = Function.Category.id;
    let dimap = Function.Profunctor.dimap;
    property1("should satisfy identity", arb_nat, (n) => {
      dimap(id, id, string_of_int)(n) == string_of_int(n);
    });
    property1("should satisfy composition", arb_nat, (n) => {
      let (f1, g1, f2, g2, a) =
        ( float_of_int, string_of_float, (*)(4), (++)("!"), (*.)(2.0) );
      (dimap(f2, g2) <. dimap(f1, g1))(a)(n) == dimap(f2 >. f1, g2 <. g1, a)(n)
    });
  });
});
