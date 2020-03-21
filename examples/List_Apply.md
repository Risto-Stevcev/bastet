```ocaml
# module List_Apply (M: Interface.MAGMA) =
  struct let apply init list = ListLabels.fold_left ~f:M.append ~init list end;;
module List_Apply :
  functor (M : Ocaml_abstract.Interface.MAGMA) ->
    sig val apply : M.t -> M.t list -> M.t end

# module List_Additive_Int_Apply = List_Apply(Int.Additive.Magma);;
module List_Additive_Int_Apply : sig val apply : int -> int list -> int end

# module List_Subtractive_Int_Apply = List_Apply(Int.Subtractive.Magma);;
module List_Subtractive_Int_Apply : sig val apply : int -> int list -> int end

# let adder = List_Additive_Int_Apply.apply;;
val adder : int -> int list -> int = <fun>

# let subtractor = List_Subtractive_Int_Apply.apply;;
val subtractor : int -> int list -> int = <fun>

# [1; 2; 3] |> adder 0 = (0 + 1 + 2 + 3);;
- : bool = true

# [1; 2; 3] |> subtractor 10 = (10 - 1 - 2 - 3);;
- : bool = true
```
