module Quickcheck :
  Bastet.Test.QUICKCHECK
    with type t = unit Alcotest.test_case
     and type 'a arbitrary = 'a QCheck.arbitrary = struct
  type t = unit Alcotest.test_case

  type 'a arbitrary = 'a QCheck.arbitrary

  let arbitrary_int = QCheck.int

  let default_count = 100

  let default_name = ""

  let property ?(count = default_count) ?(name = default_name) arb cb =
    QCheck.Test.make ~count ~name arb cb |> QCheck_alcotest.to_alcotest

  let property2 ?(count = default_count) ?(name = default_name) arb_a arb_b cb =
    QCheck.Test.make ~count ~name (QCheck.pair arb_a arb_b) (fun (a, b) -> cb a b)
    |> QCheck_alcotest.to_alcotest

  let property3 ?(count = default_count) ?(name = default_name) arb_a arb_b arb_c cb =
    QCheck.Test.make ~count ~name (QCheck.triple arb_a arb_b arb_c) (fun (a, b, c) -> cb a b c)
    |> QCheck_alcotest.to_alcotest

  let property4 ?(count = default_count) ?(name = default_name) arb_a arb_b arb_c arb_d cb =
    QCheck.Test.make ~count ~name (QCheck.quad arb_a arb_b arb_c arb_d) (fun (a, b, c, d) ->
        cb a b c d)
    |> QCheck_alcotest.to_alcotest
end
