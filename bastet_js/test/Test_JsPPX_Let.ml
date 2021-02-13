open BsMocha.Mocha
open! BsMocha.Async
open BsChai.Expect.Expect
open BsChai.Expect.Combos.End

let ( <. ) = Function.Infix.( <. )

external now : unit -> float = "now" [@@bs.module "perf_hooks"] [@@bs.scope "performance"]

;;
describe "PPX_Let" (fun () ->
    let module Monad : Interface.MONAD with type 'a t = ('a -> unit) -> unit = struct
      type 'a t = ('a -> unit) -> unit

      let map f ta cb = ta (cb <. f)

      let apply tf ta cb =
        let state = ref `None in
        tf (fun f ->
            match !state with
            | `Some_A a -> cb (f a)
            | _ -> state := `Some_F f);
        ta (fun a ->
            match !state with
            | `Some_F f -> cb (f a)
            | _ -> state := `Some_A a)

      let pure a cb = cb a

      let flat_map ta f cb = ta (fun a -> f a cb)
    end in
    let module PPX_Let = PPX_Let.Make (Monad) in
    let open PPX_Let.Let_syntax in
    let delayed_cb delay cb = Js.Global.setTimeout (cb <. now) delay |> ignore in
    let fast_cb = delayed_cb 100 in
    let slow_cb = delayed_cb 200 in
    let expect_first matcher (a, b) = expect a |> matcher b in
    describe "bind" (fun () ->
        let bind_both ta tb = bind ta (fun a -> bind tb (fun b -> return (a, b))) in
        it "should resolve first callback first when it resolves faster" (fun done_ ->
            bind_both fast_cb slow_cb (done_ <. expect_first to_be_below));
        it "should resolve first callback first when it resolves slower" (fun done_ ->
            bind_both slow_cb fast_cb (done_ <. expect_first to_be_below)));
    describe "both" (fun () ->
        it "should resolve first callback first when it resolves faster" (fun done_ ->
            both fast_cb slow_cb (done_ <. expect_first to_be_below));
        it "should resolve first callback second when it resolves slower" (fun done_ ->
            both slow_cb fast_cb (done_ <. expect_first to_be_above))))
