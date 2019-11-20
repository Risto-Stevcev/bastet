open BsMocha.Mocha;
open! BsMocha.Async;
open BsChai.Expect.Expect;
open BsChai.Expect.Combos.End;
let (<.) = Function.Infix.(<.);

[@bs.module "perf_hooks"] [@bs.scope "performance"]
external now: unit => float = "now";

describe("PPX_Let", () => {
  module Monad: Interface.MONAD with type t('a) = ('a => unit) => unit = {
    type t('a) = ('a => unit) => unit;
    let map = (f, ta, cb) => ta(cb <. f);
    let apply = (tf, ta, cb) => {
      let state = ref(`None);
      tf(f =>
        switch (state^) {
        | `Some_A(a) => cb(f(a))
        | _ => state := `Some_F(f)
        }
      );
      ta(a =>
        switch (state^) {
        | `Some_F(f) => cb(f(a))
        | _ => state := `Some_A(a)
        }
      );
    };
    let pure = (a, cb) => cb(a);
    let flat_map = (ta, f, cb) => ta(a => f(a, cb));
  };
  module PPX_Let = PPX_Let.Make(Monad);
  open PPX_Let.Let_syntax;

  let delayed_cb = (delay, cb) =>
    Js.Global.setTimeout(cb <. now, delay) |> ignore;
  let fast_cb = delayed_cb(100);
  let slow_cb = delayed_cb(200);
  let expect_first = (matcher, (a, b)) => expect(a) |> matcher(b);

  describe("bind", () => {
    let bind_both = (ta, tb) => bind(ta, a => bind(tb, b => return((a, b))));
    it("should resolve first callback first when it resolves faster", done_ =>
      bind_both(fast_cb, slow_cb, done_ <. expect_first(to_be_below))
    );
    it("should resolve first callback first when it resolves slower", done_ =>
      bind_both(slow_cb, fast_cb, done_ <. expect_first(to_be_below))
    );
  });

  describe("both", () => {
    it("should resolve first callback first when it resolves faster", done_ =>
      both(fast_cb, slow_cb, done_ <. expect_first(to_be_below))
    );
    it("should resolve first callback second when it resolves slower", done_ =>
      both(slow_cb, fast_cb, done_ <. expect_first(to_be_above))
    );
  });
});
