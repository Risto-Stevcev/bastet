open Interface

module Magma (M : MAGMA) = struct
  let ( <:> ) = M.append
end

module Magma_Any (M : MAGMA_ANY) = struct
  let ( <:> ) = M.append
end

module Functor (F : FUNCTOR) = struct
  let ( <$> ) = F.map

  and ( <@> ) f x = F.map x f
end

module Apply (A : APPLY) = struct
  include Functor (A)

  let ( <*> ) = A.apply
end

module Monad (M : MONAD) = struct
  include Apply (M)

  let ( >>= ) = M.flat_map

  and ( =<< ) ma f = M.flat_map f ma

  let ( >=> ) f g a = f a >>= g

  and ( <=< ) f g a = f =<< g a
end

module Alt (A : ALT) = struct
  include Functor (A)

  let ( <|> ) = A.alt
end

module Alternative (A : ALTERNATIVE) = struct
  include Alt (A)
  include Apply (A)
end

module Semigroupoid (S : SEMIGROUPOID) = struct
  let ( <. ) = S.compose

  and ( >. ) g f = S.compose f g
end

module Eq (E : EQ) = struct
  let ( =|= ) = E.eq
end

module Ord (O : ORD) = struct
  let ( <|| ), ( ||> ), ( <|= ), ( >|= ) =
    let module Fn = Ordering (O) in
    Fn.less_than, Fn.greater_than, Fn.less_than_or_equal, Fn.greater_than_or_equal
end

module Semiring (S : SEMIRING) = struct
  let ( |+| ) = S.add

  and ( |*| ) = S.multiply
end

module Ring (R : RING) = struct
  include Semiring (R)

  let ( |-| ) = R.subtract
end

module Euclidean_Ring (E : EUCLIDEAN_RING) = struct
  include Ring (E)

  let ( |/| ) = E.divide

  and ( |%| ) = E.modulo
end

module Extend (E : EXTEND) = struct
  let ( <<= ) = E.extend

  and ( =>> ) a f = E.extend f a
end

module Bifunctor (B : BIFUNCTOR) = struct
  let ( <<$>> ) = B.bimap
end

module Biapply (B : BIAPPLY) = struct
  include Bifunctor (B)

  let ( <<*>> ) = B.biapply
end

module Join_Semilattice (J : JOIN_SEMILATTICE) = struct
  let ( <||> ) = J.join
end

module Meet_Semilattice (M : MEET_SEMILATTICE) = struct
  let ( <&&> ) = M.meet
end

module Heyting_Algebra (H : HEYTING_ALGEBRA) = struct
  let ( --> ) = H.implies
end
