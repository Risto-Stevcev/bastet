module ArbitraryString :
  Bastet.Test.ARBITRARY with type t = string and type 'a arbitrary = 'a QCheck.arbitrary = struct
  type t = string

  type 'a arbitrary = 'a QCheck.arbitrary

  let make = QCheck.string
end

module TestString = Bastet.Test.String (AlcotestI.Test) (QCheckI.Quickcheck) (ArbitraryString)

let suites = TestString.suites
