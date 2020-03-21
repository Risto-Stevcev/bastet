```ocaml
# #require "ocaml_abstract";;
# open Ocaml_abstract;;

# module Hush (B : Interface.BIFUNCTOR) =
  struct let hush bifunctor = bifunctor |> B.bimap Function.Category.id (Function.const ()) end;;
module Hush :
  functor (B : Ocaml_abstract.Interface.BIFUNCTOR) ->
    sig val hush : ('a, 'b) B.t -> ('a, unit) B.t end

# module Hush_Result = Hush (Result.Bifunctor)
module Hush_Result : sig val hush : ('a, 'b) result -> ('a, unit) result end

# let hush = Hush_Result.hush
val hush : ('a, 'b) result -> ('a, unit) result = <fun>
```
