open BsMocha.Mocha
open BsJsverify.Verify.Arbitrary
open BsJsverify.Verify.Property

let ( <. ) =
  let open Function.Infix in
  ( <. )

(** Note: Promises are not actually Monads because you can't have
    Js.Promise.t(Js.Promise.t('a))
    Even though it's a valid bucklescript signature *)

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
          (Obj.magic <. V.homomorphism string_of_int)))
