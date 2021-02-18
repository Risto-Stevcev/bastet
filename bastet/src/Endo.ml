open Interface

let id = Function.Category.id

let ( <. ) = Function.Infix.( <. )

type 'a endo = Endo of ('a -> 'a)

module Magma : MAGMA_ANY with type 'a t = 'a endo = struct
  type 'a t = 'a endo

  let append (Endo f) (Endo g) = Endo (f <. g)
end

module Semigroup : SEMIGROUP_ANY with type 'a t = 'a endo = struct
  include Magma
end

module Monoid : MONOID_ANY with type 'a t = 'a endo = struct
  include Semigroup

  let empty = Endo id
end

module Infix = struct
  include Infix.Magma_Any (Magma)
end
