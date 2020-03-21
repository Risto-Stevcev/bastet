module Quickcheck:
  Bastet.Test.QUICKCHECK with
    type t = Alcotest.test_case(unit) and
    type arbitrary('a) = QCheck.arbitrary('a) = {
  type t = Alcotest.test_case(unit);
  type arbitrary('a) = QCheck.arbitrary('a);

  let arbitrary_int = QCheck.int;

  let default_count = 100;
  let default_name = "";

  let property = (~count=default_count, ~name=default_name, arb, cb) =>
    QCheck.Test.make(~count, ~name, arb, cb) |> QCheck_alcotest.to_alcotest;

  let property2 = (~count=default_count, ~name=default_name, arb_a, arb_b, cb) =>
    QCheck.Test.make(~count, ~name, QCheck.pair(arb_a, arb_b), ((a, b)) =>
      cb(a, b)
    )
    |> QCheck_alcotest.to_alcotest;

  let property3 =
      (~count=default_count, ~name=default_name, arb_a, arb_b, arb_c, cb) =>
    QCheck.Test.make(
      ~count, ~name, QCheck.triple(arb_a, arb_b, arb_c), ((a, b, c)) =>
      cb(a, b, c)
    )
    |> QCheck_alcotest.to_alcotest;

  let property4 =
      (
        ~count=default_count,
        ~name=default_name,
        arb_a,
        arb_b,
        arb_c,
        arb_d,
        cb,
      ) =>
    QCheck.Test.make(
      ~count, ~name, QCheck.quad(arb_a, arb_b, arb_c, arb_d), ((a, b, c, d)) =>
      cb(a, b, c, d)
    )
    |> QCheck_alcotest.to_alcotest;
};
