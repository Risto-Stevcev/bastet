# bs-abstract

Bucklescript interfaces and implementations for category theory and abstract algebra

<img src="https://raw.githubusercontent.com/Risto-Stevcev/bs-abstract/master/cantellated_tesseract.png" height="100" width="100"/>

## Installation

Install the project:

`npm install bs-abstract --save`

And add the dependency to your bs-dependencies in `bsconfig.json`:

```json
"bs-dependencies": [
  "bs-abstract"
]
```

The project will be available under the `BsAbstract` namespace

## Project Layout

This is the current layout of the project. It's subject to change:

- [src/interfaces/Interface.re][1] - Contains the category theory and abstract algebra interfaces
- [src/interfaces/Verify.re][2] - Contains property based tests to verify that implementations are lawful
- [src/interfaces/Infix.re][3] - Contains functors to generate infix operators for the interfaces. Modules implementing interfaces contain an already instantiated Infix module for convenience where appropriate
- [src/utilities/Default.re][4] - Contains default implementations for interface functions
- [src/utilities/Functors.re][5] - Contains already instantiated functors for common data combinations for convenience
- [src/functions/Functions.re][6] - Contains generic functions that are built on top the abstract interfaces
- [src/implementations/][7] - Contains implementations for common bucklescript types 

The rest of the files under `src` are implementations based on data type (ie: `String.re` for strings). These files and their corresponding unit tests in the `test` folder will give you an idea on how to use and implement the interfaces for your own data structures.

## Suggested Usage

- The suggested way to combine monadic code is to use kliesli composition instead of `flat_map`. For example, given a 
  type that's a monad, a very common pattern is to get the inner value and pass it in as an argument to a 
  subsequent function, like so:

  ```reason
  module I = Functions.Infix.Monad(BsEffects.Effect.Monad);

  let exclaim_file = path => BsEffects.Effect.Infix.({
    read_file(path) >>= contents => {
      write_file(path, contents ++ "!")
    }
  });
  ```

  Which looks like this using do notation (in haskell):

  ```haskell
  contents <- read_file "foo"
  _ <- write_file "foo" (contents ++ "!")
  ```

  This can be written with kliesli composition like this:

  ```reason
  module Effect_Infix = Functions.Infix.Monad(BsEffects.Effect.Monad);
  let ((>=>), (>.)) = (Effect_Infix.(>=>), Function.Infix.(>.));

  let exclaim = Function.flip((++))("!");
  let exclaim_file = path => Function.const(read_file(path)) >=> (exclaim >. write_file(path));
  ```

  Building up functions using function and kliesli composition is a good litmus test that your program 
  is built up from generic, pure abstractions. Which means that the code is easy to abstract to make it reusable in many 
  other contexts, and abstractions are easy to decompose when requirements change.


- For interfaces based on functors, Use already instantiated functors if available to avoid the extra boilerplate, ie:
  ```reason
  ArrayF.Int.Additive.Fold_Map.fold_map
  ```

- Don't overuse infix operators. If the code is combinatorial it can make it more readable, but a lot of times prefix operators are simpler and easier to read
- If you do use infix operators, prefer local opens over global opens, and prefer explicit unpacking over local opens, ie:

  ```reason
  let ((<.), (>.)) = Function.Infix.((<.), (>.))
  ```

- Abbreviated modules can make code terser and easier to read in some situations (ie: `A.map`), especially in situations where infix operators can't be used because they would introduce ambiguity, like for example when two different monoids are used in the same function.


Example code:
```reason
module T = ListF.Option.Traversable;
assert(T.sequence([Some("foo"), Some("bar")]) == Some(["foo", "bar"]));
Js.log(ListF.Int.Show.show([1,1,2,3,5,8]));
```

See the unit tests for many more examples

## Side effects / IO

See the [bs-effects][8] package for sync and async implementations of the "IO monad", and 
the [bs-free][9] package for free monads and other free structures.

## Use with ppx_let

You can integrate monads with [ppx_let](https://opam.ocaml.org/packages/ppx_let/), a ppx rewriter that provides 
"do notation" sugar for monads. The rewriter expects a `Let_syntax` module to be in scope, which you can construct 
using `PPX_Let.Make`, like so: 

```ocaml
module OptionLet = PPX_Let.Make(Option.Monad);;

let add_optionals = fun x y ->
  let open OptionLet in
  let%bind x' = x in 
  let%bind y' = y in
  Some (x' + y');;

Js.log @@ add_optionals (Some 123) (Some 456);; (* Some 579 *)
```

Currently as of this writing, there's no support for `let%bind` style syntax for ReasonML, but it 
should be available in one of the next releases


## License

Licensed under the BSD-3-Clause license. See `LICENSE`



[1]: https://github.com/Risto-Stevcev/bs-abstract/blob/master/src/interfaces/Interface.re
[2]: https://github.com/Risto-Stevcev/bs-abstract/blob/master/src/interfaces/Verify.re
[3]: https://github.com/Risto-Stevcev/bs-abstract/blob/master/src/interfaces/Infix.re
[4]: https://github.com/Risto-Stevcev/bs-abstract/blob/master/src/utilities/Default.re
[5]: https://github.com/Risto-Stevcev/bs-abstract/blob/master/src/utilities/Functors.re
[6]: https://github.com/Risto-Stevcev/bs-abstract/blob/master/src/functions/Functions.re
[7]: https://github.com/Risto-Stevcev/bs-abstract/blob/master/src/implementations
[8]: https://github.com/Risto-Stevcev/bs-effects
[9]: https://github.com/Risto-Stevcev/bs-free
