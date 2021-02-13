open BsMocha.Mocha
open BsJsverify.Verify.Arbitrary
open BsJsverify.Verify.Property

let ( <. ) =
  let open Function.Infix in
  ( <. )

module ComparePromise = struct
  type 'a t = 'a Js.Promise.t

  let eq a b =
    Js.Promise.then_ (fun a' -> Js.Promise.then_ (fun b' -> Js.Promise.resolve (a' = b')) b) a
    |> Obj.magic
end

;;
describe "Promise" (fun () ->
    let promise a =
      Js.Promise.make (fun ~resolve ~reject:_ ->
          Js.Global.setTimeout (fun () -> (resolve a [@bs])) 10 |> ignore)
    in
    describe "Functor" (fun () ->
        let module V = Verify.Compare.Functor (Promise.Functor) (ComparePromise) in
        async_property1 "should satisfy identity" arb_nat (Obj.magic <. V.identity <. promise);
        async_property1
          "should satisfy composition"
          arb_nat
          (Obj.magic <. V.composition (( ^ ) "!") string_of_int <. promise));
    describe "Apply" (fun () ->
        let module V = Verify.Compare.Apply (Promise.Apply) (ComparePromise) in
        async_property1
          "should satisfy associative composition"
          arb_nat
          (Obj.magic
          <. V.associative_composition
               (Js.Promise.resolve (( ^ ) "!"))
               (Js.Promise.resolve string_of_int)
          <. promise));
    describe "Applicative" (fun () ->
        let module V = Verify.Compare.Applicative (Promise.Applicative) (ComparePromise) in
        async_property1 "should satisfy identity" arb_nat (Obj.magic <. V.identity <. promise);
        async_property1
          "should satisfy homomorphism"
          arb_nat
          (Obj.magic <. V.homomorphism string_of_int));
    describe "Monad (unlawful)" (fun () ->
        let module V = Verify.Compare.Monad (Promise.Monad) (ComparePromise) in
        let module Fn = Functions.Monad (Promise.Monad) in
        let pure = Promise.Applicative.pure in
        async_property1
          "should satisfy associativity"
          arb_nat
          (Obj.magic <. V.associativity (pure <. string_of_int) (pure <. ( ^ ) "!") <. promise);
        async_property1
          "should satisfy identity"
          arb_nat
          (Obj.magic <. V.identity (pure <. string_of_int));
        async_property1 "will *seemingly* properly flatten (not correct)" arb_nat (fun n ->
            let open Promise.Infix in
            Fn.flatten (pure (pure n)) >>= fun flattened_n -> pure (flattened_n = n));
        async_property1
          "promises are not actually nested despite the type signature"
          arb_nat
          (fun n ->
            let open Promise.Infix in
            pure (pure n)
            >>= (fun result -> pure (result >>= (pure <. Function.const false)))
            >>= Function.Category.id
            |> Js.Promise.catch (pure <. Function.const true))))
