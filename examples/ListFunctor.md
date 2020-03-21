```ocaml
# let id x = x;;
val id : 'a -> 'a = <fun>

# let compose f g x = f (g x);;
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun>

# let satisfies_identity = List.map id [1; 2; 3] = [1; 2; 3];;
val satisfies_identity : bool = true

# let satisfies_composition =
  List.map (compose string_of_int ((+) 1)) [1; 2; 3] =
  compose (List.map string_of_int) (List.map ((+) 1)) [1; 2; 3];;
val satisfies_composition : bool = true
```
