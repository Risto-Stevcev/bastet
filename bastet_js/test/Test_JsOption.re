open BsMocha.Mocha;
open BsJsverify.Verify.Arbitrary;

module ArbitraryOption:
  Test.ARBITRARY_A with
    type t('a) = option('a) and type arbitrary('a) = arbitrary('a) = {
  type t('a) = option('a);
  type nonrec arbitrary('a) = arbitrary('a);
  let make = arb_option;
  let make_bound = arb_option;
};

module ArbitraryOptionInt:
  Test.ARBITRARY with
    type t = option(int) and type arbitrary('a) = arbitrary('a) = {
  type t = option(int);
  type nonrec arbitrary('a) = arbitrary('a);
  let make = arb_option(arb_int(-1000, 1000));
};

module TestOption =
  Test.Option(
    MochaI.Test,
    JsVerifyI.Quickcheck,
    ArbitraryOptionInt,
    ArbitraryOption,
  );

describe("Option", () => {
  MochaI.run(TestOption.suites)
});
