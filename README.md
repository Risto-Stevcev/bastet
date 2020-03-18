# ocaml-abstract

A ReasonML/Ocaml library for category theory and abstract algebra.

<img src="https://raw.githubusercontent.com/Risto-Stevcev/bs-abstract/master/cantellated_tesseract.png" height="100" width="100"/>


## Documentation

See [documentation][1]


## Installation

Install the project:

`npm install ocaml-abstract --save`

And add the dependency to your bs-dependencies in `bsconfig.json`:

```json
"bs-dependencies": [
  "ocaml-abstract"
]
```

The project will be available under the `BsAbstract` namespace


## Examples

```ocaml
# #require "ocaml_abstract";;
# open Ocaml_abstract;;
# module T = Functors.ListF.Option.Traversable;;
module T = Ocaml_abstract.Functors.ListF.Option.Traversable

# T.sequence [Some "foo"; Some "bar"];;
- : string list option = Some ["foo"; "bar"]

# Functors.ListF.Int.Show.show [1; 1; 2; 3; 5; 8];;
- : string = "[1, 1, 2, 3, 5, 8]"
```

## Suggested Usage

### Use kliesli composition

The monadic equivalent of the `|>` operator is `flat_map` (`>>=`). Both are very useful for writing
code that's easy to reason about as data all flows in one direction.

However, function composition (`>.`) and kliesli composition for monads (`>=>`) are often underused
in code. Composing functions and monads monoidally like this in a concatenative style can make a
codebase easier to read and can also prevent devs on the team from duplicating code.

Consider a typical use case. Often in a codebase you have something that fetches a value that may or
may not exist (`'a option`).  Splitting out this code into smaller functions and combining (and
reusing) them with composition leads a lean code style:

```ocaml
# let ((>=>)) = Option.Infix.((>=>));;
val ( >=> ) : ('a -> 'b option) -> ('b -> 'c option) -> 'a -> 'c option =
  <fun>

# type form = { name: string; address: string option };;
type form = { name : string; address : string option; }

# let get_form () =
  (* Assume some side effect got the form here *)
  Some { name = "Foo"; address = Some "123 Bar St." };;
val get_form : unit -> form option = <fun>

# let get_address form = form.address;;
val get_address : form -> string option = <fun>

# let get_form_address = get_form >=> get_address;;
val get_form_address : unit -> string option = <fun>

# get_form_address ();;
- : string option = Some "123 Bar St."
```

### Instantiated Functors

For interfaces based on functors, use already instantiated functors if available to avoid the extra
boilerplate:

```ocaml
# Functors.ArrayF.Int.Additive.Fold_Map.fold_map
- : ('a -> int) -> 'a array -> int = <fun>
```

### Don't Overuse Infix

Don't overuse infix operators. If the code is combinatorial it can make it more readable, but in a
lot of cases the prefix operators are simpler and easier to read. If you do use infix operators,
prefer local opens over global opens to avoid polluting the toplevel:

```ocaml
# let trim_all strings =
  let open List.Infix in
  StringLabels.trim <$> strings;;
val trim_all : string list -> string list = <fun>

# trim_all ["foo "; "bar"; "   baz"];;
- : string list = ["foo"; "bar"; "baz"]
```

### Use Abbreviated Modules

Abbreviated modules can make code both terser and easier to read in some situations, like for
example where two different semigroups are used in the same function and infix operators can't be
used:

```ocaml
# type game = { score: int; disqualified: bool };;
type game = { score : int; disqualified : bool; }

# let total_score a b =
  let module I = Int.Additive.Semigroup in
  let module B = Bool.Disjunctive.Semigroup in
  { score = I.append a.score b.score; disqualified = B.append a.disqualified b.disqualified };;
val total_score : game -> game -> game = <fun>

# let result =
  let game_1 = { score = 4; disqualified = false }
  and game_2 = { score = 2; disqualified = true }
  in
  total_score game_1 game_2;;
val result : game = {score = 6; disqualified = true}
```


## License

See [LICENSE][2]


[1]: https://risto-stevcev.github.io/ocaml-abstract
[2]: https://github.com/Risto-Stevcev/bs-abstract/blob/master/LICENSE
