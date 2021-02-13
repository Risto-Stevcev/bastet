open BsMocha.Mocha
open BsChai.Expect.Expect
open BsChai.Expect.Combos.End
open BsJsverify.Verify.Arbitrary
open BsJsverify.Verify.Property

let ( <. ), ( >. ) =
  let open Function.Infix in
  ( <. ), ( >. )

;;
describe "Function" (fun () ->
    describe "Functor" (fun () ->
        property1 "should satisfy identity" arb_nat (fun n ->
            let module V =
              Verify.Compare.Functor
                (Function.Functor (struct
                  type t = int
                end))
                (struct
                  type 'a t = int -> 'a

                  let eq = Obj.magic (fun a b -> a n = b n)
                end)
            in
            V.identity string_of_int);
        property1 "should satisfy composition" arb_nat (fun n ->
            let module V =
              Verify.Compare.Functor
                (Function.Functor (struct
                  type t = int
                end))
                (struct
                  type 'a t = int -> 'a

                  let eq = Obj.magic (fun a b -> a n = b n)
                end)
            in
            V.composition (( ^ ) "!") string_of_int (( + ) 1)));
    describe "Apply" (fun () ->
        property1 "should satisfy associative composition" arb_nat (fun n ->
            let module V =
              Verify.Compare.Apply
                (Function.Apply (struct
                  type t = int
                end))
                (struct
                  type 'a t = int -> 'a

                  let eq = Obj.magic (fun a b -> a n = b n)
                end)
            in
            V.associative_composition ( + ) ( - ) (( + ) 1));
        it "should compose two infix functions (x - x * y)" (fun () ->
            let module Apply_Fn_Int = Function.Apply (struct
              type t = int
            end) in
            let module Apply_Util = Functions.Apply (Apply_Fn_Int) in
            let fn = Apply_Util.lift2 Function.Semigroupoid.compose ( - ) ( * ) in
            expect (fn 3 4) |> to_be (-9)));
    describe "Semigroupoid" (fun () ->
        property1 "should satisfy associativity" arb_nat (fun n ->
            let module V =
              Verify.Compare.Semigroupoid
                (Function.Semigroupoid)
                (struct
                  type ('a, 'b) t = 'a -> 'b

                  let eq = Obj.magic (fun a b -> a n = b n)
                end)
            in
            V.associativity (( = ) "123!") (( ^ ) "!") string_of_int));
    describe "Category" (fun () ->
        property1 "should satisfy identity" arb_nat (fun n ->
            let module V =
              Verify.Compare.Category
                (Function.Category)
                (struct
                  type ('a, 'b) t = 'a -> 'b

                  let eq = Obj.magic (fun a b -> a n = b n)
                end)
            in
            V.identity string_of_int));
    describe "Profunctor" (fun () ->
        property1 "should satisfy identity" arb_nat (fun n ->
            let module V =
              Verify.Compare.Profunctor
                (Function.Profunctor)
                (struct
                  type ('a, 'b) t = 'a -> 'b

                  let eq = Obj.magic (fun a b -> a n = b n)
                end)
            in
            V.identity string_of_int);
        property1 "should satisfy composition" arb_nat (fun n ->
            let module V =
              Verify.Compare.Profunctor
                (Function.Profunctor)
                (struct
                  type ('a, 'b) t = 'a -> 'b

                  let eq = Obj.magic (fun a b -> a n = b n)
                end)
            in
            V.composition float_of_int Js.Float.toString (( * ) 4) (( ^ ) "!") (( *. ) 2.0)));
    describe "Contravariant" (fun () ->
        property1 "should satisfy identity" arb_nat (fun n ->
            let module V =
              Verify.Compare.Contravariant
                (Function.Contravariant (struct
                  type t = string
                end))
                (struct
                  type 'a t = 'a -> string

                  let eq = Obj.magic (fun a b -> a n = b n)
                end)
            in
            V.identity string_of_int);
        property1 "should satisfy composition" arb_nat (fun n ->
            let module V =
              Verify.Compare.Contravariant
                (Function.Contravariant (struct
                  type t = string
                end))
                (struct
                  type 'a t = 'a -> string

                  let eq = Obj.magic (fun a b -> a n = b n)
                end)
            in
            V.composition (( + ) 1) string_of_int (( ^ ) "!")));
    describe "Bicontravariant" (fun () ->
        property2 "should satisfy identity" arb_nat arb_nat (fun n1 n2 ->
            let module V =
              Verify.Compare.Bicontravariant
                (Function.Bicontravariant (struct
                  type t = int
                end))
                (struct
                  type ('a, 'b) t = 'a -> 'b -> int

                  let eq = Obj.magic (fun a b -> a n1 n2 = b n1 n2)
                end)
            in
            V.identity ( + ));
        property2 "should satisfy composition" arb_nat arb_nat (fun n1 n2 ->
            let module V =
              Verify.Compare.Bicontravariant
                (Function.Bicontravariant (struct
                  type t = int
                end))
                (struct
                  type ('a, 'b) t = 'a -> 'b -> int

                  let eq = Obj.magic (fun a b -> a n1 n2 = b n1 n2)
                end)
            in
            V.composition (( + ) 1) (( + ) 2) (( * ) 3) (( * ) 4) ( + ))))
