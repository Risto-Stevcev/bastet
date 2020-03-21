```ocaml
# let empty = true;;
val empty : bool = true

# let satisfies_identity_law = (false && empty) = false;;
val satisfies_identity_law : bool = true

# let satisfies_associative_law = (true && (true && false)) = ((true && true) && false);;
val satisfies_associative_law : bool = true
```
