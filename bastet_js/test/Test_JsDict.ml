open BsMocha.Mocha
open BsChai.Expect.Expect
open BsChai.Expect.Combos.End
open BsJsverify.Verify.Arbitrary
open BsJsverify.Verify.Property

let id = Function.Category.id

let ( <. ) = Function.Infix.( <. )

;;
describe "Dict" (fun () ->
    describe "Functor" (fun () ->
        let module V = Verify.Functor (Dict.Functor) in
        property1 "should satisfy identity" (arb_dict arb_nat) V.identity;
        property1 "should satisfy composition" (arb_dict arb_nat) (fun a ->
            V.composition (( ^ ) "!") string_of_int a));
    describe "Apply" (fun () ->
        let module V = Verify.Apply (Dict.Apply) in
        property1 "should satisfy associative composition" (arb_dict arb_nat) (fun n ->
            V.associative_composition
              (Js.Dict.fromList ["g", ( ^ ) "!"])
              (Js.Dict.fromList ["f", string_of_int])
              n));
    describe "Alt" (fun () ->
        let module V = Verify.Alt (Dict.Alt) in
        property3
          "should satisfy associativity"
          (arb_dict arb_nat)
          (arb_dict arb_nat)
          (arb_dict arb_nat)
          V.associativity;
        property2
          "should satisfy distributivity"
          (arb_dict arb_nat)
          (arb_dict arb_nat)
          (V.distributivity string_of_int));
    describe "Plus" (fun () ->
        let module V = Verify.Plus (Dict.Plus) in
        it "should satisfy annihalation" (fun () ->
            expect (V.annihalation string_of_int) |> to_be true);
        property1 "should satisfy identity" (arb_dict arb_nat) V.identity);
    describe "Foldable" (fun () ->
        it "should do a left fold" (fun () ->
            expect
              (Dict.Foldable.fold_left
                 ( + )
                 0
                 (Dict.unsafe_from_object [%bs.obj { a = 1; b = 2; c = 3 }]))
            |> to_be 6;
            expect
              (Dict.Foldable.fold_left
                 ( - )
                 10
                 (Dict.unsafe_from_object [%bs.obj { a = 1; b = 3; c = 4 }]))
            |> to_be 2));
    describe "Traversable" (fun () ->
        let module T = Dict.Traversable (Option.Applicative) in
        it "should sequence the dict" (fun () ->
            expect
              (T.sequence
                 (Dict.unsafe_from_object
                    [%bs.obj
                      {
                        a = Some 123 [@explicit_arity];
                        b = Some 456 [@explicit_arity];
                        c = Some 789 [@explicit_arity];
                      }]))
            |> to_be
                 (Some (Dict.unsafe_from_object [%bs.obj { a = 123; b = 456; c = 789 }])
                 [@explicit_arity]);
            expect
              (T.sequence
                 (Dict.unsafe_from_object
                    [%bs.obj
                      { a = Some 123 [@explicit_arity]; b = None; c = Some 789 [@explicit_arity] }]))
            |> to_be None)))
