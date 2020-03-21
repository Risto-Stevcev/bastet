```ocaml
# let empty = "";;
val empty : string = ""

# let satisfies_identity_law = "foo" ^ empty = "foo";;
val satisfies_identity_law : bool = true

# let satisfies_associative_law = "foo" ^ ("bar" ^ "baz") = ("foo" ^ "bar") ^ "baz";;
val satisfies_associative_law : bool = true
```
