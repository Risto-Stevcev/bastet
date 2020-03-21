open BsMocha.Mocha;
open BsJsverify.Verify.Arbitrary;

module ArbitraryString:
  Test.ARBITRARY with type t = string and type arbitrary('a) = arbitrary('a) = {
  type t = string;
  type nonrec arbitrary('a) = arbitrary('a);
  let make = arb_string;
};

module TestString =
  Test.String(MochaI.Test, JsVerifyI.Quickcheck, ArbitraryString);

describe("String", () => {
  MochaI.run(TestString.suites)
});
