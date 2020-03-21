```ocaml
# let id x = x;;
val id : 'a -> 'a = <fun>

# let compose f g x = f (g x);;
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun>

# let map f = function
  | Some value -> Some (f value)
  | None -> None;;
val map : ('a -> 'b) -> 'a option -> 'b option = <fun>

# let satisfies_identity = map id (Some 123) = Some 123;;
val satisfies_identity : bool = true

# let satisfies_composition =
  map (compose string_of_int ((+) 1)) (Some 123) =
  compose (map string_of_int) (map ((+) 1)) (Some 123);;
val satisfies_composition : bool = true
```
