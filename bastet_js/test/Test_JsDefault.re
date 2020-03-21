open BsMocha.Mocha;

module TestDefault = Test.Default(MochaI.Test, JsVerifyI.Quickcheck);

describe("Default", () => {
  MochaI.run(TestDefault.suites)
});
