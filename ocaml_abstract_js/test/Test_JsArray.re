open BsMocha.Mocha;
open BsJsverify.Verify.Arbitrary;
let id = Function.Category.id;
let (<.) = Function.Infix.(<.);

module ArbitraryArray:
  Test.ARBITRARY_A with
    type t('a) = array('a) and type arbitrary('a) = arbitrary('a) = {
  type t('a) = array('a);
  type nonrec arbitrary('a) = arbitrary('a);
  let make = arb_array;
  let make_bound = arb_array;
};

module ArbitraryArrayInt:
  Test.ARBITRARY with
    type t = array(int) and type arbitrary('a) = arbitrary('a) = {
  type t = array(int);
  type nonrec arbitrary('a) = arbitrary('a);
  let make = arb_array(arb_int(-1000, 1000));
};

module TestArray =
  Test.Array(
    MochaI.Test,
    JsVerifyI.Quickcheck,
    ArbitraryArray,
    ArbitraryArrayInt,
  );

describe("Array", () => {
  MochaI.run(TestArray.suites)
});
