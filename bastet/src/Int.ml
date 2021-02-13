open Interface

module Additive = struct
  module Magma : MAGMA with type t = int = struct
    type t = int

    let append = ( + )
  end

  module Medial_Magma : MEDIAL_MAGMA with type t = int = Magma

  module Semigroup : SEMIGROUP with type t = int = struct
    include Magma
  end

  module Monoid : MONOID with type t = int = struct
    include Semigroup

    let empty = 0
  end

  module Quasigroup : QUASIGROUP with type t = int = struct
    include Magma
  end

  module Medial_Quasigroup : MEDIAL_QUASIGROUP with type t = int = struct
    include Medial_Magma
  end

  module Loop : LOOP with type t = int = struct
    include Quasigroup

    let empty = 0
  end

  module Group : GROUP with type t = int = struct
    include Monoid

    let inverse = ( * ) (-1)
  end

  module Abelian_Group : ABELIAN_GROUP with type t = int = struct
    include Group
  end
end

module Multiplicative = struct
  module Magma : MAGMA with type t = int = struct
    type t = int

    let append = ( * )
  end

  module Medial_Magma : MEDIAL_MAGMA with type t = int = Magma

  module Semigroup : SEMIGROUP with type t = int = struct
    include Magma
  end

  module Monoid : MONOID with type t = int = struct
    include Semigroup

    let empty = 1
  end

  module Quasigroup : QUASIGROUP with type t = int = struct
    include Magma
  end

  module Loop : LOOP with type t = int = struct
    include Quasigroup

    let empty = 1
  end
end

module Subtractive = struct
  module Magma : MAGMA with type t = int = struct
    type t = int

    let append = ( - )
  end

  module Medial_Magma : MEDIAL_MAGMA with type t = int = Magma

  module Quasigroup : QUASIGROUP with type t = int = struct
    include Magma
  end
end

module Divisive = struct
  module Magma : MAGMA with type t = int = struct
    type t = int

    let append = ( / )
  end
end

module Eq : EQ with type t = int = struct
  type t = int

  let eq = ( = )
end

module Ord : ORD with type t = int = struct
  include Eq

  let compare = unsafe_compare
end

module Bounded : BOUNDED with type t = int = struct
  include Ord

  let top = max_int

  let bottom = min_int
end

module Show : SHOW with type t = int = struct
  type t = int

  let show = string_of_int
end

module Semiring : SEMIRING with type t = int = struct
  type t = int

  let add = ( + )

  let zero = 0

  let multiply = ( * )

  let one = 1
end

module Ring : RING with type t = int = struct
  include Semiring

  let subtract = ( - )
end

module Commutative_Ring : COMMUTATIVE_RING with type t = int = struct
  include Ring
end

module Euclidean_Ring : EUCLIDEAN_RING with type t = int = struct
  include Commutative_Ring

  let degree a = min (abs a) Bounded.top

  let divide = ( / )

  let modulo a b = a mod b
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
