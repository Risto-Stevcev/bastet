module ArbitraryOption :
  Bastet.Test.ARBITRARY_A with type 'a t = 'a option and type 'a arbitrary = 'a QCheck.arbitrary =
struct
  type 'a t = 'a option

  type 'a arbitrary = 'a QCheck.arbitrary

  let make = QCheck.option

  let make_bound value = QCheck.option value
end

module ArbitraryOptionInt :
  Bastet.Test.ARBITRARY with type t = int option and type 'a arbitrary = 'a QCheck.arbitrary =
struct
  type t = int option

  type 'a arbitrary = 'a QCheck.arbitrary

  let make = QCheck.option QCheck.int
end

module TestOption =
  Bastet.Test.Option (AlcotestI.Test) (QCheckI.Quickcheck) (ArbitraryOptionInt) (ArbitraryOption)

let suites = TestOption.suites
