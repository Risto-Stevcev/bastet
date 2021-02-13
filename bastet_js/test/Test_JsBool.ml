open BsMocha.Mocha
open BsJsverify.Verify.Arbitrary

module ArbitraryBool : Test.ARBITRARY with type t = bool and type 'a arbitrary = 'a arbitrary =
struct
  type t = bool

  type nonrec 'a arbitrary = 'a arbitrary

  let make = arb_bool
end

module TestBool = Test.Bool (MochaI.Test) (JsVerifyI.Quickcheck) (ArbitraryBool)

;;
describe "Bool" (fun () -> MochaI.run TestBool.suites)
