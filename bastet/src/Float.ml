open Interface

(** Note: `float` is not a fully law abiding member of Additive.Semigroup,
    Multiplicative.Semigroup, Semiring, Division_Ring, and Euclidean_Ring, and any
    abstractions dependent on these, due to potential arithmetic overflows and
    floating point precision issues *)

let approximately_equal =
  (fun ~tolerance a b -> abs_float (a -. b) <= tolerance
    : tolerance:float -> float -> float -> bool)

module Additive = struct
  module Magma : MAGMA with type t = float = struct
    type t = float

    let append = ( +. )
  end

  module Medial_Magma : MEDIAL_MAGMA with type t = float = Magma

  module Semigroup : SEMIGROUP with type t = float = struct
    include Magma
  end

  module Monoid : MONOID with type t = float = struct
    include Semigroup

    let empty = 0.0
  end

  module Quasigroup : QUASIGROUP with type t = float = struct
    include Magma
  end

  module Medial_Quasigroup : MEDIAL_QUASIGROUP with type t = float = struct
    include Medial_Magma
  end

  module Loop : LOOP with type t = float = struct
    include Quasigroup

    let empty = 0.0
  end

  module Group : GROUP with type t = float = struct
    include Monoid

    let inverse = ( *. ) (-1.0)
  end

  module Abelian_Group : ABELIAN_GROUP with type t = float = struct
    include Group
  end
end

module Multiplicative = struct
  module Magma : MAGMA with type t = float = struct
    type t = float

    let append = ( *. )
  end

  module Medial_Magma : MEDIAL_MAGMA with type t = float = Magma

  module Semigroup : SEMIGROUP with type t = float = struct
    include Magma
  end

  module Monoid : MONOID with type t = float = struct
    include Semigroup

    let empty = 1.0
  end

  module Quasigroup : QUASIGROUP with type t = float = struct
    include Magma
  end

  module Medial_Quasigroup : MEDIAL_QUASIGROUP with type t = float = struct
    include Medial_Magma
  end

  module Loop : LOOP with type t = float = struct
    include Quasigroup

    let empty = 1.0
  end
end

module Subtractive = struct
  module Magma : MAGMA with type t = float = struct
    type t = float

    let append = ( -. )
  end

  module Medial_Magma : MEDIAL_MAGMA with type t = float = Magma

  module Quasigroup : QUASIGROUP with type t = float = struct
    include Magma
  end

  module Medial_Quasigroup : MEDIAL_QUASIGROUP with type t = float = struct
    include Medial_Magma
  end
end

module Divisive = struct
  module Magma : MAGMA with type t = float = struct
    type t = float

    let append = ( /. )
  end

  module Medial_Magma : MEDIAL_MAGMA with type t = float = Magma

  module Quasigroup : QUASIGROUP with type t = float = struct
    include Magma
  end

  module Medial_Quasigroup : MEDIAL_QUASIGROUP with type t = float = struct
    include Medial_Magma
  end
end

module Eq : EQ with type t = float = struct
  type t = float

  let eq = ( = )
end

module Ord : ORD with type t = float = struct
  include Eq

  let compare = unsafe_compare
end

module Bounded : BOUNDED with type t = float = struct
  include Ord

  let top = max_float

  and bottom = min_float
end

module Show : SHOW with type t = float = struct
  type t = float

  let show = string_of_float
end

module Semiring : SEMIRING with type t = float = struct
  type t = float

  let add = ( +. )

  let zero = 0.0

  let multiply = ( *. )

  let one = 1.0
end

module Ring : RING with type t = float = struct
  include Semiring

  let subtract = ( -. )
end

module Commutative_Ring : COMMUTATIVE_RING with type t = float = struct
  include Ring
end

module Division_Ring : DIVISION_RING with type t = float = struct
  include Ring

  let reciprocal a = 1.0 /. a
end

module Euclidean_Ring : EUCLIDEAN_RING with type t = float = struct
  include Commutative_Ring

  let degree _ = 1

  let divide = ( /. )

  let modulo _ _ = 0.0
end

module Field : FIELD with type t = float = struct
  include Euclidean_Ring

  include (Division_Ring : DIVISION_RING with type t := t)
end

module Infix = struct
  module Additive = struct
    include Infix.Magma (Additive.Magma)
  end

  module Multiplicative = struct
    include Infix.Magma (Multiplicative.Magma)
  end

  include Infix.Eq (Eq)
  include Infix.Ord (Ord)
  include Infix.Euclidean_Ring (Euclidean_Ring)
end
