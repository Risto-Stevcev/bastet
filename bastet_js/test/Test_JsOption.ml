open BsMocha.Mocha
open BsJsverify.Verify.Arbitrary

module ArbitraryOption :
  Test.ARBITRARY_A with type 'a t = 'a option and type 'a arbitrary = 'a arbitrary = struct
  type 'a t = 'a option

  type nonrec 'a arbitrary = 'a arbitrary

  let make = arb_option

  let make_bound = arb_option
end

module ArbitraryOptionInt :
  Test.ARBITRARY with type t = int option and type 'a arbitrary = 'a arbitrary = struct
  type t = int option

  type nonrec 'a arbitrary = 'a arbitrary

  let make = arb_option (arb_int (-1000) 1000)
end

module TestOption =
  Test.Option (MochaI.Test) (JsVerifyI.Quickcheck) (ArbitraryOptionInt) (ArbitraryOption)

;;
describe "Option" (fun () -> MochaI.run TestOption.suites)
