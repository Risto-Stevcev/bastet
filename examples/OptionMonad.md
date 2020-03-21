```ocaml
# let compose f g x = f (g x);;
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun>

# let pure a = Some a;;
val pure : 'a -> 'a option = <fun>

# let map f = function
  | Some value -> Some (f value)
  | None -> None;;
val map : ('a -> 'b) -> 'a option -> 'b option = <fun>

# let flatten: 'a option option -> 'a option = function
  | Some value -> value
  | None -> None;;
val flatten : 'a option option -> 'a option = <fun>

# let flat_map f = compose flatten (map f);;
val flat_map : ('a -> 'b option) -> 'a option -> 'b option = <fun>

# let (>>=) a b = flat_map b a;;
val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option = <fun>


# (* Another style of working with monads instead of flat_map is to use
     kliesli composition: *)
  let kliesli_compose f g a = f a >>= g;;
val kliesli_compose :
  ('a -> 'b option) -> ('b -> 'c option) -> 'a -> 'c option = <fun>

# let (>=>) = kliesli_compose;;
val ( >=> ) : ('a -> 'b option) -> ('b -> 'c option) -> 'a -> 'c option =
  <fun>


# (* Here's an example use case of option types as monads - getting deeply nested
     optional properties: *);;

# type user_details = { phone_number: int option; address: string option };;
type user_details = { phone_number : int option; address : string option; }

# type user = { name: string; details: user_details option };;
type user = { name : string; details : user_details option; }

# let fetch_user name =
  match name with
  | "Risto" ->
    Some { name; details = Some { phone_number = None; address = Some "123 Foo St" } }
  | _ -> None;;
val fetch_user : string -> user option = <fun>

# let get_details user = user.details;;
val get_details : user -> user_details option = <fun>

# let get_address details = details.address;;
val get_address : user_details -> string option = <fun>

# (* With monads, getting values from deeply nested optional values is trivial: *)
  fetch_user "Risto" >>= get_details >>= get_address;;
- : string option = Some "123 Foo St"

# (* It's also trivial to turn this into a function with kliesli composition,
     which can makes things easier to read: *)
  let fetch_user_address = fetch_user >=> get_details >=> get_address;;
val fetch_user_address : string -> string option = <fun>

# fetch_user_address "Risto";;
- : string option = Some "123 Foo St"
```
