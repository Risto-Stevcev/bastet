## Potentially breaking changes

- `Float.Show` now uses `string_of_float` for cross-compatibility. This might be undesirable
  depending on your use cases, in which case `JsFloat.Show` is provided which uses
  `Js.Float.toString`. See [bucklescript#3412][1].

- The `JsArray` module is provided for users that want to have the transpiled bucklescript code use
  the builtin `Array.prototype` api instead of the version used for Ocaml compatibility. Using this
  on the frontend would mean potentially smaller bundle sizes (implementations are built into JS),
  and possible performance improvements since the javascript engines know explicitly what the code
  is trying to do.

- `Result.Unsafe` functions now raise an Ocaml `Invalid_argument` error instead of a JS type error
  on failure. No js compat is currently provided since these are unsafe functions to begin with and
  shouldn't be used.


## Notes

These are updates that shouldn't cause any breaking changes but are documentated here

- `Int.EuclideanRing` also uses the builtin `min` function now. This functions behaves identically
  to the js counterpart `Js.Math.min`, it's just noted here for reference. 

- `Option.getWithDefault` copies the same implementation from bucklescript's stdlib so that it works
  on native as well.


[1]: https://github.com/BuckleScript/bucklescript/issues/3412
