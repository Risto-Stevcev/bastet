module Test:
  Test.TEST with
    type test = unit => unit and
    type suite(_) = unit => unit and
    type check('a) = BsJsverify.Verify.Arbitrary.arbitrary('a) = {
  type test = unit => unit;
  type suite(_) = unit => unit;
  type check('a) = BsJsverify.Verify.Arbitrary.arbitrary('a);

  let int = BsJsverify.Verify.Arbitrary.arb_int(-1000, 1000);
  let string = BsJsverify.Verify.Arbitrary.arb_string;
  let array = Obj.magic;
  let list = Obj.magic;
  let option = Obj.magic;
  let tuple = (a, b) => Obj.magic((a, b));

  let check = (check, ~name="", expected, actual) => {
    check |> ignore;
    BsMocha.Assert.deep_equal(~message=name, expected, actual);
  };
  let test = (name, fn, ()) => BsMocha.Mocha.it(name, fn);
  let suite = (name, tests, ()) =>
    BsMocha.Mocha.describe(name, () => {
      ListLabels.iter(~f=cb => cb(), tests)
    });
};

let run = suites => suites |> ListLabels.iter(~f=cb => cb());
