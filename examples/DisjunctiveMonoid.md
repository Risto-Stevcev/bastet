```ocaml
# let empty = false;;
val empty : bool = false

# let satisfies_identity_law = (true || empty) = true;;
val satisfies_identity_law : bool = true

# let satisfies_associative_law = (true || (true || false)) = ((true || true) || false);;
val satisfies_associative_law : bool = true
```
