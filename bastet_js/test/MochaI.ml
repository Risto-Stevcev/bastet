module Test :
  Test.TEST
    with type test = unit -> unit
     and type _ suite = unit -> unit
     and type 'a check = 'a BsJsverify.Verify.Arbitrary.arbitrary = struct
  type test = unit -> unit

  type _ suite = unit -> unit

  type 'a check = 'a BsJsverify.Verify.Arbitrary.arbitrary

  let int = BsJsverify.Verify.Arbitrary.arb_int (-1000) 1000

  let bool = BsJsverify.Verify.Arbitrary.arb_bool

  let string = BsJsverify.Verify.Arbitrary.arb_string

  let array = Obj.magic

  let list = Obj.magic

  let option = Obj.magic

  let tuple a b = Obj.magic (a, b)

  let check check ?(name = "") expected actual =
    check |> ignore;
    BsMocha.Assert.deep_equal ~message:name expected actual

  let test name fn () = BsMocha.Mocha.it name fn

  let suite name tests () =
    BsMocha.Mocha.describe name (fun () -> ListLabels.iter ~f:(fun cb -> cb ()) tests)
end

let run suites = suites |> ListLabels.iter ~f:(fun cb -> cb ())
