open BsMocha.Mocha
open BsJsverify.Verify.Arbitrary

module ArbitraryInt : Test.ARBITRARY with type t = int and type 'a arbitrary = 'a arbitrary = struct
  type t = int

  type nonrec 'a arbitrary = 'a arbitrary

  let make =
    arb_int
      (Js.Math.pow_int ~base:Int.Bounded.bottom ~exp:(1 / 4))
      (Js.Math.pow_int ~base:Int.Bounded.top ~exp:(1 / 4))
end

module TestInt = Test.Int (MochaI.Test) (JsVerifyI.Quickcheck) (ArbitraryInt)

;;
describe "Int" (fun () -> MochaI.run TestInt.suites)
