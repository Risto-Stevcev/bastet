open BsMocha.Mocha;
open BsChai.Expect.Expect;
open BsChai.Expect.Combos.End;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
let id = Function.Category.id;
let (<.) = Function.Infix.(<.);

describe("JsDict", () => {
  describe("Functor", () => {
    module V = Verify.Functor(JsDict.Functor);
    property1("should satisfy identity", arb_dict(arb_nat), V.identity);
    property1("should satisfy composition", arb_dict(arb_nat), a =>
      V.composition((++)("!"), string_of_int, a)
    );
  });

  describe("Apply", () => {
    module V = Verify.Apply(JsDict.Apply);
    property1("should satisfy associative composition", arb_dict(arb_nat), n =>
      V.associative_composition(
        Js.Dict.fromList([("g", (++)("!"))]),
        Js.Dict.fromList([("f", string_of_int)]),
        n,
      )
    );
  });

  describe("Alt", () => {
    module V = Verify.Alt(JsDict.Alt);
    property3(
      "should satisfy associativity",
      arb_dict(arb_nat),
      arb_dict(arb_nat),
      arb_dict(arb_nat),
      V.associativity,
    );
    property2(
      "should satisfy distributivity",
      arb_dict(arb_nat),
      arb_dict(arb_nat),
      V.distributivity(string_of_int),
    );
  });

  describe("Plus", () => {
    module V = Verify.Plus(JsDict.Plus);
    it("should satisfy annihalation", () =>
      expect(V.annihalation(string_of_int)) |> to_be(true)
    );
    property1("should satisfy identity", arb_dict(arb_nat), V.identity);
  });

  describe("Foldable", () =>
    it("should do a left fold", () => {
      expect(
        JsDict.Foldable.fold_left(
          (+),
          0,
          JsDict.unsafe_from_object({"a": 1, "b": 2, "c": 3}),
        ),
      )
      |> to_be(6);
      expect(
        JsDict.Foldable.fold_left(
          (-),
          10,
          JsDict.unsafe_from_object({"a": 1, "b": 3, "c": 4}),
        ),
      )
      |> to_be(2);
    })
  );

  describe("Traversable", () => {
    module T = JsDict.Traversable(Option.Applicative);

    it("should sequence the dict", () => {
      expect(
        T.sequence(
          JsDict.unsafe_from_object({
            "a": Some(123),
            "b": Some(456),
            "c": Some(789),
          }),
        ),
      )
      |> to_be(
           Some(JsDict.unsafe_from_object({"a": 123, "b": 456, "c": 789})),
         );

      expect(
        T.sequence(
          JsDict.unsafe_from_object({
            "a": Some(123),
            "b": None,
            "c": Some(789),
          }),
        ),
      )
      |> to_be(None);
    });
  });
});
