module Test:
  Bastet.Test.TEST with
    type test = Alcotest.test_case(unit) and
    type suite('a) = (string, list('a)) and
    type check('a) = Alcotest.testable('a) = {
  type test = Alcotest.test_case(unit);
  type suite('a) = (string, list('a));
  type check('a) = Alcotest.testable('a);

  let int = Alcotest.int;
  let bool = Alcotest.bool;
  let string = Alcotest.string;
  let array = Alcotest.array;
  let list = Alcotest.list;
  let option = Alcotest.option;
  let tuple = Alcotest.pair;

  let check = (check, ~name="", expected, actual) =>
    Alcotest.check(check, name, expected, actual);
  let test = (name, fn) => Alcotest.test_case(name, `Quick, fn);
  let suite = (name, tests) => (name, tests);
};
