[@bs.val] external describe : (string, unit => unit) => unit = "describe";

[@bs.val] external it : (string, unit => unit) => unit = "it";

[@bs.module "assert"] external eq : ('a, 'a) => unit = "deepEqual";

[@bs.module "assert"] external neq : ('a, 'a) => unit = "notDeepEqual";

[@bs.module "assert"] external should_be_ok : Js.boolean => unit = "ok";

[@bs.module "assert"]
external fail :
  (~actual: 'a, ~expected: 'a, ~message: string=?, ~operator: string=?, unit) => unit =
  "fail";

type expect('a) = {
  to_be: 'a => unit,
  to_not_be: 'a => unit,
  to_fail: 'a => unit
};

let expect = (actual) => {
  to_be: (expected) => eq(actual, expected),
  to_not_be: (expected) => neq(actual, expected),
  to_fail: (expected) => fail(~actual, ~expected, ())
};
