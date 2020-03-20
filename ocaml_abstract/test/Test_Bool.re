module ArbitraryBool:
  Ocaml_abstract.Test.ARBITRARY with
    type t = bool and type arbitrary('a) = QCheck.arbitrary('a) = {
  type t = bool;
  type arbitrary('a) = QCheck.arbitrary('a);
  let make = QCheck.bool;
};

module TestBool =
  Ocaml_abstract.Test.Bool(AlcotestI.Test, QCheckI.Quickcheck, ArbitraryBool);

let suites = TestBool.suites;
