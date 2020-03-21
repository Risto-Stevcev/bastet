open BsMocha.Mocha;
open BsJsverify.Verify.Arbitrary;

module ArbitraryInt:
  Test.ARBITRARY with type t = int and type arbitrary('a) = arbitrary('a) = {
  type t = int;
  type nonrec arbitrary('a) = arbitrary('a);

  // These bounds ensure that there are no arithmetic overflows
  let make =
    arb_int(
      Js.Math.pow_int(~base=Int.Bounded.bottom, ~exp=1 / 4),
      Js.Math.pow_int(~base=Int.Bounded.top, ~exp=1 / 4),
    );
};

module TestInt = Test.Int(MochaI.Test, JsVerifyI.Quickcheck, ArbitraryInt);

describe("Int", () => {
  MochaI.run(TestInt.suites)
});
