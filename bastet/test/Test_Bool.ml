module ArbitraryBool :
  Bastet.Test.ARBITRARY with type t = bool and type 'a arbitrary = 'a QCheck.arbitrary = struct
  type t = bool

  type 'a arbitrary = 'a QCheck.arbitrary

  let make = QCheck.bool
end

module TestBool = Bastet.Test.Bool (AlcotestI.Test) (QCheckI.Quickcheck) (ArbitraryBool)

let suites = TestBool.suites
