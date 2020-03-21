open BsMocha.Mocha;
open BsJsverify.Verify.Arbitrary;

module ArbitraryBool:
  Test.ARBITRARY with type t = bool and type arbitrary('a) = arbitrary('a) = {
  type t = bool;
  type nonrec arbitrary('a) = arbitrary('a);
  let make = arb_bool;
};

module TestBool = Test.Bool(MochaI.Test, JsVerifyI.Quickcheck, ArbitraryBool);

describe("Bool", () => {
  MochaI.run(TestBool.suites)
});
