module ArbitraryArray :
  Bastet.Test.ARBITRARY_A with type 'a t = 'a array and type 'a arbitrary = 'a QCheck.arbitrary =
struct
  type 'a t = 'a array

  type 'a arbitrary = 'a QCheck.arbitrary

  let make = QCheck.array

  let make_bound value = QCheck.array_of_size (QCheck.Gen.int_bound 100) value
end

module ArbitraryArrayInt :
  Bastet.Test.ARBITRARY with type t = int array and type 'a arbitrary = 'a QCheck.arbitrary = struct
  type t = int array

  type 'a arbitrary = 'a QCheck.arbitrary

  let make = QCheck.array QCheck.int
end

module TestArray =
  Bastet.Test.Array (Bastet.Array) (AlcotestI.Test) (QCheckI.Quickcheck) (ArbitraryArrayInt)
    (ArbitraryArray)

let suites = TestArray.suites
