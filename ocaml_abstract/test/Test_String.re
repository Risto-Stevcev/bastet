module ArbitraryString:
  Ocaml_abstract.Test.ARBITRARY with
    type t = string and type arbitrary('a) = QCheck.arbitrary('a) = {
  type t = string;
  type arbitrary('a) = QCheck.arbitrary('a);
  let make = QCheck.string;
};

module TestString =
  Ocaml_abstract.Test.String(
    AlcotestI.Test,
    QCheckI.Quickcheck,
    ArbitraryString,
  );

let suites = TestString.suites;
