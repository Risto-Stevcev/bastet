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

- [src/Interface.re][1] - Contains the category theory and abstract algebra interfaces
- [src/Verify.re][2] - Contains property based tests to verify that implementations are lawful
- [src/Infix.re][3] - Contains functors to generate infix operators for the interfaces. Modules implementing interfaces contain an already instantiated Infix module for convenience where appropriate
- [src/Default.re][4] - Contains default implementations for interface functions
- [src/Functions.re][5] - Contains generic functions that are built on top the abstract interfaces
- [src/Functors.re][6] - Contains already instantiated functors for common data combinations for convenience

The rest of the files under `src` are implementations based on data type (ie: `String.re` for strings). These files and their corresponding unit tests in the `test` folder will give you an idea on how to use and implement the interfaces for your own data structures.

## Suggested Usage

- For interfaces based on functors, Use already instantiated functors if available to avoid the extra boilerplate (ie: `ArrayF.Int.Additive.Fold_Map.fold_map`)
- Don't overuse infix operators. If the code is combinatorial it can make it more readable, but a lot of times prefix operators are simpler and easier to read
- If you do use infix operators, prefer local opens over global opens, and prefer explicit unpacking over local opens (ie: `let ((<.), (>.)) = Function.Infix.((<.), (>.))`)
- Abbreviated modules can make code terser and easier to read in some situations (ie: `A.map`), especially in situations where infix operators can't be used because they would introduce ambiguity, like for example when two different monoids are used in the same function.


Example code:
```reason
module A = ListF.Option.Traversable;
assert(A.sequence([Some("foo"), Some("bar")]) == Some(["foo", "bar"]));
Js.log(ListF.Int.Show.show([1,1,2,3,5,8]));
```

See the unit tests for many more examples


## License

Licensed under the BSD-3-Clause license. See `LICENSE`



[1]: https://github.com/Risto-Stevcev/bs-abstract/blob/master/src/Interface.re
[2]: https://github.com/Risto-Stevcev/bs-abstract/blob/master/src/Verify.re
[3]: https://github.com/Risto-Stevcev/bs-abstract/blob/master/src/Infix.re
[4]: https://github.com/Risto-Stevcev/bs-abstract/blob/master/src/Default.re
[5]: https://github.com/Risto-Stevcev/bs-abstract/blob/master/src/Functions.re
[6]: https://github.com/Risto-Stevcev/bs-abstract/blob/master/src/Functors.re
