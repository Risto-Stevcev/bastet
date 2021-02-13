open BsMocha.Mocha
open BsJsverify.Verify.Arbitrary

module ArbitraryFloat : Test.ARBITRARY with type t = float and type 'a arbitrary = 'a arbitrary =
struct
  type t = float

  type nonrec 'a arbitrary = 'a arbitrary

  let make = arb_float Float.Bounded.bottom (Float.Bounded.top ** (1. /. 150.))
end

module ApproximatelyEq = struct
  type t = float

  let approx = Js.Float.toPrecisionWithPrecision ~digits:4

  let eq a b = approx a = approx b
end

module TestFloat =
  Test.Float (ApproximatelyEq) (MochaI.Test) (JsVerifyI.Quickcheck) (ArbitraryFloat)

;;
describe "Float" (fun () -> MochaI.run TestFloat.suites)
