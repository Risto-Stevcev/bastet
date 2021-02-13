module ArbitraryList :
  Bastet.Test.ARBITRARY_A with type 'a t = 'a list and type 'a arbitrary = 'a QCheck.arbitrary =
struct
  type 'a t = 'a list

  type 'a arbitrary = 'a QCheck.arbitrary

  let make = QCheck.list

  let make_bound value = QCheck.list_of_size (QCheck.Gen.int_bound 100) value
end

module ArbitraryListInt :
  Bastet.Test.ARBITRARY with type t = int list and type 'a arbitrary = 'a QCheck.arbitrary = struct
  type t = int list

  type 'a arbitrary = 'a QCheck.arbitrary

  let make = QCheck.list QCheck.int
end

module TestList =
  Bastet.Test.List (AlcotestI.Test) (QCheckI.Quickcheck) (ArbitraryListInt) (ArbitraryList)

let suites = TestList.suites
