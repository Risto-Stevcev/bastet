open Interface

module Magma : MAGMA with type t = string = struct
  type t = string

  let append = ( ^ )
end

module Semigroup : SEMIGROUP with type t = string = struct
  include Magma
end

module Monoid : MONOID with type t = string = struct
  include Semigroup

  let empty = ""
end

module Quasigroup : QUASIGROUP with type t = string = struct
  include Magma
end

module Loop : LOOP with type t = string = struct
  include Quasigroup

  let empty = ""
end

module Eq : EQ with type t = string = struct
  type t = string

  let eq = ( = )
end

module Ord : ORD with type t = string = struct
  include Eq

  let compare = unsafe_compare
end

module Show : SHOW with type t = string = struct
  type t = string

  let show = Function.Category.id
end

module Infix = struct
  include Infix.Magma (Magma)
  include Infix.Eq (Eq)
  include Infix.Ord (Ord)
end
