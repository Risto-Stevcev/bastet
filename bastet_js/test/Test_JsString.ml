open BsMocha.Mocha
open BsJsverify.Verify.Arbitrary

module ArbitraryString : Test.ARBITRARY with type t = string and type 'a arbitrary = 'a arbitrary =
struct
  type t = string

  type nonrec 'a arbitrary = 'a arbitrary

  let make = arb_string
end

module TestString = Test.String (MochaI.Test) (JsVerifyI.Quickcheck) (ArbitraryString)

;;
describe "String" (fun () -> MochaI.run TestString.suites)
