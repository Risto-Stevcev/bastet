module ArbitraryFloat :
  Bastet.Test.ARBITRARY with type t = float and type 'a arbitrary = 'a QCheck.arbitrary = struct
  type t = float

  type 'a arbitrary = 'a QCheck.arbitrary

  let make =
    QCheck.float_range Bastet.Float.Bounded.bottom (Bastet.Float.Bounded.top ** (1. /. 150.))
end

module ApproximatelyEq = struct
  type t = float

  let approx = Printf.sprintf "%.4f"

  let eq a b = approx a = approx b
end

module TestFloat =
  Bastet.Test.Float (ApproximatelyEq) (AlcotestI.Test) (QCheckI.Quickcheck) (ArbitraryFloat)

let suites = TestFloat.suites
