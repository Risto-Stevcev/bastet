open BsMocha.Mocha
open BsChai.Expect.Expect
open BsChai.Expect.Combos.End
open BsJsverify.Verify.Arbitrary
open BsJsverify.Verify.Property
open Functors

let ( <. ) = Function.Infix.( <. )

;;
describe "Tuple" (fun () ->
    describe "Semigroup" (fun () ->
        let module S = Tuple.Semigroup (String.Semigroup) (Int.Additive.Semigroup) in
        let module V = Verify.Semigroup (S) in
        property3
          "should satisfy associativity"
          (arb_tuple (arb_string, arb_nat))
          (arb_tuple (arb_string, arb_nat))
          (arb_tuple (arb_string, arb_nat))
          V.associativity);
    describe "Monoid" (fun () ->
        let module M = Tuple.Monoid (String.Monoid) (Int.Additive.Monoid) in
        let module V = Verify.Monoid (M) in
        property1 "should satisfy identity" (arb_tuple (arb_string, arb_nat)) V.identity);
    describe "Functor" (fun () ->
        let module V = Verify.Functor (TupleF.String.Functor) in
        property1 "should satisfy identity" (arb_tuple (arb_string, arb_nat)) V.identity;
        property1
          "should satisfy composition"
          (arb_tuple (arb_string, arb_nat))
          (fun a -> V.composition (( ^ ) "!") string_of_int a));
    describe "Apply" (fun () ->
        let module V = Verify.Apply (TupleF.String.Apply) in
        property1
          "should satisfy associative composition"
          (arb_tuple (arb_string, arb_nat))
          (fun n ->
            V.associative_composition
              (String.Monoid.empty, ( ^ ) "!")
              (String.Monoid.empty, string_of_int)
              n));
    describe "Applicative" (fun () ->
        let module V = Verify.Applicative (TupleF.String.Applicative) in
        property1 "should satisfy identity" (arb_tuple (arb_string, arb_nat)) V.identity;
        property1
          "should satisfy homomorphism"
          (arb_tuple (arb_string, arb_nat))
          (V.homomorphism (TupleF.String.Functor.map string_of_int));
        property1
          "should satisfy interchange"
          arb_nat
          (V.interchange (String.Monoid.empty, string_of_int)));
    describe "Monad" (fun () ->
        let module V = Verify.Monad (TupleF.String.Monad) in
        let pure = TupleF.String.Applicative.pure in
        property1
          "should satisfy associativity"
          (arb_tuple (arb_string, arb_nat))
          (V.associativity (pure <. string_of_int) (pure <. ( ^ ) "!"));
        property1 "should satisfy identity" arb_nat (V.identity (pure <. string_of_int)));
    describe "Foldable" (fun () ->
        let open TupleF.String.Foldable in
        let empty = String.Monoid.empty in
        it "should do a left fold" (fun () ->
            expect (fold_left ( + ) 0 (empty, 123)) |> to_be 123;
            expect (fold_left ( - ) 10 (empty, 321)) |> to_be (-311));
        it "should do a right fold" (fun () ->
            expect (fold_right ( - ) 10 (empty, 321)) |> to_be 311);
        it "should do a map fold (int)" (fun () ->
            let module F = Fold_Map (Int.Additive.Monoid) in
            expect (F.fold_map Function.Category.id (empty, 123)) |> to_be 123);
        it "should do a map fold (list)" (fun () ->
            let module F = Fold_Map_Plus (List.Plus) in
            expect (F.fold_map List.Applicative.pure (empty, [1; 2; 3])) |> to_be [[1; 2; 3]]));
    describe "Traversable" (fun () ->
        let traverse, sequence =
          let open TupleF.String.List.Traversable in
          traverse, sequence
        in
        it "should traverse the array" (fun () ->
            let positive_int x =
              match x >= 0 with
              | true -> [x]
              | false -> []
            in
            expect (traverse positive_int ("foo", 123)) |> to_be ["foo", 123];
            expect (traverse positive_int ("bar", -123)) |> to_be []);
        it "should sequence the array" (fun () ->
            expect (sequence ("foo", [123])) |> to_be ["foo", 123];
            expect (sequence ("bar", [])) |> to_be []));
    describe "Eq" (fun () ->
        let module E = Tuple.Eq (String.Eq) (Int.Eq) in
        let module V = Verify.Eq (E) in
        property1 "should satisfy reflexivity" (arb_tuple (arb_string, arb_nat)) V.reflexivity;
        property2
          "should satisfy symmetry"
          (arb_tuple (arb_string, arb_nat))
          (arb_tuple (arb_string, arb_nat))
          V.symmetry;
        property3
          "should satisfy transitivity"
          (arb_tuple (arb_string, arb_nat))
          (arb_tuple (arb_string, arb_nat))
          (arb_tuple (arb_string, arb_nat))
          V.transitivity);
    describe "Bifunctor" (fun () ->
        let module V = Verify.Bifunctor (Tuple.Bifunctor) in
        property1 "should satisfy identity" (arb_tuple (arb_string, arb_nat)) V.identity;
        property1
          "should satisfy composition"
          (arb_tuple (arb_string, arb_nat))
          (V.composition (( ^ ) "!") (( *. ) 3.0) (( ^ ) "-") float_of_int)))
