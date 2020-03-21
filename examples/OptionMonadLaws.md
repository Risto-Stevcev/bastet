```ocaml
# #require "ocaml_abstract"
# open Ocaml_abstract;;

# let (>=>) = Option.Infix.(>=>);;
val ( >=> ) : ('a -> 'b option) -> ('b -> 'c option) -> 'a -> 'c option =
  <fun>

# let to_positive_int = function
  | x when x > 0 -> Some x
  | _ -> None;;
val to_positive_int : int -> int option = <fun>

# let to_even_int = function
  | x when x mod 2 = 0 -> Some x
  | _ -> None;;
val to_even_int : int -> int option = <fun>

# let empty = Option.Monad.pure;;
val empty : 'a -> 'a option = <fun>

# let satisfies_identity_law f value = (f >=> empty) value = f value;;
val satisfies_identity_law : ('a -> 'b option) -> 'a -> bool = <fun>

# let satisfies_associative_law f g h value =
  (f >=> (g >=> h)) value = ((f >=> g) >=> h) value;;
val satisfies_associative_law :
  ('a -> 'b option) -> ('b -> 'c option) -> ('c -> 'd option) -> 'a -> bool =
  <fun>

# satisfies_identity_law int_of_string_opt "123";;
- : bool = true
# satisfies_identity_law to_positive_int 123;;
- : bool = true
# satisfies_identity_law to_even_int 123;;
- : bool = true

# satisfies_associative_law int_of_string_opt to_positive_int to_even_int "123";;
- : bool = true
# satisfies_associative_law int_of_string_opt to_positive_int to_even_int "foo";;
- : bool = true

# let string_to_positive_even_int =
  int_of_string_opt >=> to_positive_int >=> to_even_int;;
val string_to_positive_even_int : string -> int option = <fun>
# string_to_positive_even_int "foo";;
- : int option = None
# string_to_positive_even_int "123";;
- : int option = None
# string_to_positive_even_int "124";;
- : int option = Some 124
```
