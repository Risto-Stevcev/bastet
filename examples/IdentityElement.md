```ocaml
# let empty = 0;;
val empty : int = 0
# 10 + empty = 10;;
- : bool = true

# let empty = 1;;
val empty : int = 1
# 5 * empty = 5;;
- : bool = true

# let empty = "";;
val empty : string = ""
# "foo" ^ empty = "foo";;
- : bool = true
```
