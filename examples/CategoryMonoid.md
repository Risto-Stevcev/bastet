```ocaml
# let empty x = x;;
val empty : 'a -> 'a = <fun>

# let append f g x = f (g x);;
val append : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun>

# let satisfies_identity_law = (append string_of_int empty) 123 = string_of_int 123;;
val satisfies_identity_law : bool = true

# let satisfies_associative_law =
  (append string_of_int (append ((+) 1) (( * ) 2))) 123 =
  (append (append string_of_int ((+) 1)) (( * ) 2)) 123;;
val satisfies_associative_law : bool = true
```
