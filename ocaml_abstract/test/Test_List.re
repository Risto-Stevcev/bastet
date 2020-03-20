module ArbitraryList:
  Ocaml_abstract.Test.ARBITRARY_A with
    type t('a) = list('a) and type arbitrary('a) = QCheck.arbitrary('a) = {
  type t('a) = list('a);
  type arbitrary('a) = QCheck.arbitrary('a);
  let make = QCheck.list;
  let make_bound = value =>
    QCheck.list_of_size(QCheck.Gen.int_bound(100), value);
};

module ArbitraryListInt:
  Ocaml_abstract.Test.ARBITRARY with
    type t = list(int) and type arbitrary('a) = QCheck.arbitrary('a) = {
  type t = list(int);
  type arbitrary('a) = QCheck.arbitrary('a);
  let make = QCheck.list(QCheck.int);
};

module TestList =
  Ocaml_abstract.Test.List(
    AlcotestI.Test,
    QCheckI.Quickcheck,
    ArbitraryListInt,
    ArbitraryList,
  );

let suites = TestList.suites;
