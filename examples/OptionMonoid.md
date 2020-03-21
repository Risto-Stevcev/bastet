```ocaml
# let empty: [ `foo ] option = None;;
val empty : [ `foo ] option = None

# let append a b =
  match a, b with
  | Some x, _ -> Some x
  | None, y -> y;;
val append : 'a option -> 'a option -> 'a option = <fun>

# let satisfies_identity_law = append (Some `foo) empty = Some `foo;;
val satisfies_identity_law : bool = true

# let satisfies_associative_law =
  append None (append (Some `foo) (Some `bar)) =
  append (append None (Some `foo)) (Some `bar);;
val satisfies_associative_law : bool = true
```
