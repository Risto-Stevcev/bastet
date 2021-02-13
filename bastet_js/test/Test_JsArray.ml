open BsMocha.Mocha
open BsJsverify.Verify.Arbitrary

;;
Bisect.Runtime.write_coverage_data_on_exit ()

module ArbitraryArray :
  Test.ARBITRARY_A with type 'a t = 'a array and type 'a arbitrary = 'a arbitrary = struct
  type 'a t = 'a array

  type nonrec 'a arbitrary = 'a arbitrary

  let make = arb_array

  let make_bound = arb_array
end

module ArbitraryArrayInt :
  Test.ARBITRARY with type t = int array and type 'a arbitrary = 'a arbitrary = struct
  type t = int array

  type nonrec 'a arbitrary = 'a arbitrary

  let make = arb_array (arb_int (-1000) 1000)
end

module TestArray =
  Test.Array (JsArray) (MochaI.Test) (JsVerifyI.Quickcheck) (ArbitraryArrayInt) (ArbitraryArray)

;;
describe "Array" (fun () -> MochaI.run TestArray.suites)
