open BsMocha.Mocha
module TestDefault = Test.Default (MochaI.Test) (JsVerifyI.Quickcheck)

;;
describe "Default" (fun () -> MochaI.run TestDefault.suites)
