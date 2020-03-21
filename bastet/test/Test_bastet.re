let () = print_newline();
Alcotest.run(
  "Ocaml_abstract",
  [
    Test_Array.suites,
    Test_Bool.suites,
    Test_Default.suites,
    Test_Float.suites,
    Test_List.suites,
    Test_Option.suites,
    Test_String.suites,
  ]
  |> List.concat,
);
