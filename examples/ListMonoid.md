```ocaml
# let empty: int list = [];;
val empty : int list = []

# let satisfies_identity_law = List.append [1; 2; 3] empty = [1; 2; 3];;
val satisfies_identity_law : bool = true

# let satisfies_associative_law =
  List.append [1] (List.append [2; 3] [4]) = List.append (List.append [1] [2; 3]) [4];;
val satisfies_associative_law : bool = true
```
