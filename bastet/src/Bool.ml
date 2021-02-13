open Interface

module Conjunctive = struct
  module Magma : MAGMA with type t = bool = struct
    type t = bool

    let append = ( && )
  end

  module Medial_Magma : MEDIAL_MAGMA with type t = bool = Magma

  module Semigroup : SEMIGROUP with type t = bool = struct
    include Magma
  end

  module Monoid : MONOID with type t = bool = struct
    include Semigroup

    let empty = true
  end
end

module Disjunctive = struct
  module Magma : MAGMA with type t = bool = struct
    type t = bool

    let append = ( || )
  end

  module Medial_Magma : MEDIAL_MAGMA with type t = bool = Magma

  module Semigroup : SEMIGROUP with type t = bool = struct
    include Magma
  end

  module Monoid : MONOID with type t = bool = struct
    include Semigroup

    let empty = false
  end
end

module Eq : EQ with type t = bool = struct
  type t = bool

  let eq = ( = )
end

module Ord : ORD with type t = bool = struct
  include Eq

  let compare = unsafe_compare
end

module Bounded : BOUNDED with type t = bool = struct
  include Ord

  let top = true

  and bottom = false
end

module Join_Semilattice : JOIN_SEMILATTICE with type t = bool = struct
  type t = bool

  let join = ( || )
end

module Meet_Semilattice : MEET_SEMILATTICE with type t = bool = struct
  type t = bool

  let meet = ( && )
end

module Bounded_Join_Semilattice : BOUNDED_JOIN_SEMILATTICE with type t = bool = struct
  include Join_Semilattice

  let bottom = false
end

module Bounded_Meet_Semilattice : BOUNDED_MEET_SEMILATTICE with type t = bool = struct
  include Meet_Semilattice

  let top = true
end

module Lattice : LATTICE with type t = bool = struct
  include Join_Semilattice

  include (Meet_Semilattice : MEET_SEMILATTICE with type t := t)
end

module Bounded_Lattice : BOUNDED_LATTICE with type t = bool = struct
  include Bounded_Join_Semilattice

  include (Bounded_Meet_Semilattice : BOUNDED_MEET_SEMILATTICE with type t := t)
end

module Distributive_Lattice : DISTRIBUTIVE_LATTICE with type t = bool = struct
  include Lattice
end

module Bounded_Distributive_Lattice : BOUNDED_DISTRIBUTIVE_LATTICE with type t = bool = struct
  include Bounded_Lattice
end

module Heyting_Algebra : HEYTING_ALGEBRA with type t = bool = struct
  include Ord

  include (Bounded_Distributive_Lattice : BOUNDED_DISTRIBUTIVE_LATTICE with type t := t)

  let not a = not a

  and implies a b = (not a) || b
end

module Involutive_Heyting_Algebra : INVOLUTIVE_HEYTING_ALGEBRA with type t = bool = struct
  include Heyting_Algebra
end

module Boolean_Algebra : BOOLEAN_ALGEBRA with type t = bool = struct
  include Heyting_Algebra
end

module Show : SHOW with type t = bool = struct
  type t = bool

  let show = string_of_bool
end

module Infix = struct
  module Conjunctive = struct
    include Infix.Magma (Conjunctive.Magma)
  end

  module Disjunctive = struct
    include Infix.Magma (Disjunctive.Magma)
  end

  include Infix.Eq (Eq)
  include Infix.Ord (Ord)
  include Infix.Join_Semilattice (Join_Semilattice)
  include Infix.Meet_Semilattice (Meet_Semilattice)
  include Infix.Heyting_Algebra (Heyting_Algebra)
end
