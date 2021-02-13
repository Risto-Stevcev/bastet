open BsMocha.Mocha
open BsJsverify.Verify.Arbitrary

module ArbitraryList :
  Test.ARBITRARY_A with type 'a t = 'a list and type 'a arbitrary = 'a arbitrary = struct
  type 'a t = 'a list

  type nonrec 'a arbitrary = 'a arbitrary

  let make = arb_list

  let make_bound = arb_list
end

module ArbitraryListInt :
  Test.ARBITRARY with type t = int list and type 'a arbitrary = 'a arbitrary = struct
  type t = int list

  type nonrec 'a arbitrary = 'a arbitrary

  let make = arb_list (arb_int (-1000) 1000)
end

module TestList = Test.List (MochaI.Test) (JsVerifyI.Quickcheck) (ArbitraryListInt) (ArbitraryList)

;;
describe "List'" (fun () -> MochaI.run TestList.suites)
