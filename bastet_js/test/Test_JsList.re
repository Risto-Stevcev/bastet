open BsMocha.Mocha;
open BsJsverify.Verify.Arbitrary;

module ArbitraryList:
  Test.ARBITRARY_A with
    type t('a) = list('a) and type arbitrary('a) = arbitrary('a) = {
  type t('a) = list('a);
  type nonrec arbitrary('a) = arbitrary('a);
  let make = arb_list;
  let make_bound = arb_list;
};

module ArbitraryListInt:
  Test.ARBITRARY with
    type t = list(int) and type arbitrary('a) = arbitrary('a) = {
  type t = list(int);
  type nonrec arbitrary('a) = arbitrary('a);
  let make = arb_list(arb_int(-1000, 1000));
};

module TestList =
  Test.List(
    MochaI.Test,
    JsVerifyI.Quickcheck,
    ArbitraryListInt,
    ArbitraryList,
  );

describe("List'", () => {
  MochaI.run(TestList.suites)
});
