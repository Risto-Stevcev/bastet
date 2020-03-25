module ArbitraryArray:
  Bastet.Test.ARBITRARY_A with
    type t('a) = array('a) and type arbitrary('a) = QCheck.arbitrary('a) = {
  type t('a) = array('a);
  type arbitrary('a) = QCheck.arbitrary('a);
  let make = QCheck.array;
  let make_bound = value =>
    QCheck.array_of_size(QCheck.Gen.int_bound(100), value);
};

module ArbitraryArrayInt:
  Bastet.Test.ARBITRARY with
    type t = array(int) and type arbitrary('a) = QCheck.arbitrary('a) = {
  type t = array(int);
  type arbitrary('a) = QCheck.arbitrary('a);
  let make = QCheck.array(QCheck.int);
};

module TestArray =
  Bastet.Test.Array(
    Bastet.Array,
    AlcotestI.Test,
    QCheckI.Quickcheck,
    ArbitraryArrayInt,
    ArbitraryArray,
  );

let suites = TestArray.suites;
