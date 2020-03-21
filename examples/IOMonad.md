```ocaml
# (* IO as a callback *)
  type 'a io = ('a -> unit) -> unit;;
type 'a io = ('a -> unit) -> unit

# let pure (value: 'a): 'a io = fun cb -> cb value;;
val pure : 'a -> 'a io = <fun>

# let map (f: 'a -> 'b) (x: 'a io): 'b io =
  fun cb -> x (fun value -> cb (f value));;
val map : ('a -> 'b) -> 'a io -> 'b io = <fun>

# let flat_map (f: 'a -> 'b io) (x: 'a io): 'b io =
  fun cb -> x (fun value -> (f value) (fun value2 -> cb value2));;
val flat_map : ('a -> 'b io) -> 'a io -> 'b io = <fun>

# let (>>=) a b = flat_map b a;;
val ( >>= ) : 'a io -> ('a -> 'b io) -> 'b io = <fun>

# let (>=>) f g a = f a >>= g;;
val ( >=> ) : ('a -> 'b io) -> ('b -> 'c io) -> 'a -> 'c io = <fun>

# (* Some stubbed functions, assume they do something async: *);;
# let some_effect (x: int): int io = fun cb -> cb (x * 2);;
val some_effect : int -> int io = <fun>

# let some_other_effect (x: int): int io = fun cb -> cb (x + 10);;
val some_other_effect : int -> int io = <fun>

# let yet_another_effect (x: int): string io = fun cb -> cb (string_of_int x);;
val yet_another_effect : int -> string io = <fun>


# let empty = pure;;
val empty : 'a -> 'a io = <fun>

# let satisfies_identity_law f arg =
  ((f >=> empty) arg) (fun value1 -> begin
    (f arg) (fun value2 -> Printf.printf "%B\n" (value1 = value2))
  end);;
val satisfies_identity_law : ('a -> 'b io) -> 'a -> unit = <fun>

# let satisfies_associative_law f g h arg =
  ((f >=> (g >=> h)) arg) (fun value1 -> begin
    (((f >=> g) >=> h) arg) (fun value2 -> Printf.printf "%B\n" (value1 = value2))
  end);;
val satisfies_associative_law :
  ('a -> 'b io) -> ('b -> 'c io) -> ('c -> 'd io) -> 'a -> unit = <fun>

# satisfies_identity_law some_effect 123;;
true
- : unit = ()
# satisfies_identity_law some_other_effect 123;;
true
- : unit = ()
# satisfies_identity_law yet_another_effect 123;;
true
- : unit = ()

# satisfies_associative_law some_effect some_other_effect yet_another_effect 123;;
true
- : unit = ()
```
