module Test :
  Bastet.Test.TEST
    with type test = unit Alcotest.test_case
     and type 'a suite = string * 'a list
     and type 'a check = 'a Alcotest.testable = struct
  type test = unit Alcotest.test_case

  type 'a suite = string * 'a list

  type 'a check = 'a Alcotest.testable

  let int = Alcotest.int

  let bool = Alcotest.bool

  let string = Alcotest.string

  let array = Alcotest.array

  let list = Alcotest.list

  let option = Alcotest.option

  let tuple = Alcotest.pair

  let check check ?(name = "") expected actual = Alcotest.check check name expected actual

  let test name fn = Alcotest.test_case name `Quick fn

  let suite name tests = name, tests
end
