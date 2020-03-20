module TestDefault =
  Ocaml_abstract.Test.Default(AlcotestI.Test, QCheckI.Quickcheck);

let suites = TestDefault.suites;
