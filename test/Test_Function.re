open BsMocha.Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
let ((<.), (>.)) = Function.Infix.((<.), (>.));


describe("Function", () => {
  describe("Semigroupoid", () => {
    property1("should satisfy associativity", arb_nat, (n) => {
      module V = Verify.Compare.Semigroupoid(Function.Semigroupoid, {
        type t('a, 'b) = 'a => 'b;
        let eq = Obj.magic((a, b) => a(n) == b(n));
      });
      V.associativity((==)("123!"), (++)("!"), string_of_int)
    });
  });

  describe("Category", () => {
    property1("should satisfy identity", arb_nat, (n) => {
      module V = Verify.Compare.Category(Function.Category, {
        type t('a, 'b) = 'a => 'b;
        let eq = Obj.magic((a, b) => a(n) == b(n));
      });
      V.identity(string_of_int)
    });
  });

  describe("Profunctor", () => {
    property1("should satisfy identity", arb_nat, (n) => {
      module V = Verify.Compare.Profunctor(Function.Profunctor, {
        type t('a, 'b) = 'a => 'b;
        let eq = Obj.magic((a, b) => a(n) == b(n));
      });
      V.identity(string_of_int)
    });
    property1("should satisfy composition", arb_nat, (n) => {
      module V = Verify.Compare.Profunctor(Function.Profunctor, {
        type t('a, 'b) = 'a => 'b;
        let eq = Obj.magic((a, b) => a(n) == b(n));
      });
      V.composition(float_of_int, string_of_float, (*)(4), (++)("!"), (*.)(2.0));
    });
  });
});
