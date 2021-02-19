(** This is a helper module to integrate `bs-abstract` with `ppx_let` *)

open Interface

(** The module structure that `ppx_let` expects to be in scope *)
module type PPX_LET = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val both : 'a t -> 'b t -> ('a * 'b) t

  module Open_on_rhs : sig
    val return : 'a -> 'a t
  end
end

(** Makes the `ppx_let` module from a monad *)
module Make (M : MONAD) = struct
  module A = Functions.Apply (M)

  module Let_syntax : PPX_LET with type 'a t = 'a M.t = struct
    type 'a t = 'a M.t

    let return = M.pure

    and bind = M.flat_map

    and map a ~f = M.map f a

    and both = A.apply_both

    module Open_on_rhs = struct
      let return = M.pure
    end
  end
end
