(** * * * * * * * * * * * * * * *
 * Common instantiated functors *
 * * * * * * * * * * * * * * * **)
 
module OcamlAbstractInt = Int
module OcamlAbstractBool = Bool
module OcamlAbstractString = String
module OcamlAbstractFloat = Float

module ArrayF = struct
  module Functions = struct
    module Travsersable = Functions.Traversable (Array.Traversable)
  end

  module Int = struct
    module Eq = Array.Eq (Int.Eq)
    module Ord = Array.Ord (Int.Ord)
    module Show = Array.Show (Int.Show)

    module Additive = struct
      module Fold_Map = Array.Foldable.Fold_Map (Int.Additive.Monoid)
    end

    module Multiplicative = struct
      module Fold_Map = Array.Foldable.Fold_Map (Int.Multiplicative.Monoid)
    end

    module Functions = struct
      module Scan = Functions.Travsersable.Scan (struct
        type t = int
      end)
    end
  end

  module Float = struct
    module Eq = Array.Eq (Float.Eq)
    module Ord = Array.Ord (Float.Ord)
    module Show = Array.Show (Float.Show)

    module Additive = struct
      module Fold_Map = Array.Foldable.Fold_Map (Float.Additive.Monoid)
    end

    module Multiplicative = struct
      module Fold_Map = Array.Foldable.Fold_Map (Float.Multiplicative.Monoid)
    end

    module Functions = struct
      module Scan = Functions.Travsersable.Scan (struct
        type t = float
      end)
    end
  end

  module Bool = struct
    module Eq = Array.Eq (Bool.Eq)
    module Ord = Array.Ord (Bool.Ord)
    module Show = Array.Show (Bool.Show)
  end

  module String = struct
    module Eq = Array.Eq (String.Eq)
    module Ord = Array.Ord (String.Ord)
  end

  module List = struct
    module Fold_Map_Plus = Array.Foldable.Fold_Map_Plus (List.Plus)
    module Traversable = Array.Traversable (List.Applicative)
  end

  module Option = struct
    module Fold_Map_Plus = Array.Foldable.Fold_Map_Plus (Option.Plus)
    module Traversable = Array.Traversable (Option.Applicative)
  end

  module Infix = struct
    module Functor = Infix.Functor (Array.Functor)
    module Apply = Infix.Apply (Array.Apply)
    module Monad = Infix.Monad (Array.Monad)
    module Alt = Infix.Alt (Array.Alt)
  end

  module Array = struct
    module Traversable = Array.Traversable (Array.Applicative)
  end
end

module ListF = struct
  module Functions = struct
    module Travsersable = Functions.Traversable (List.Traversable)
  end

  module Int = struct
    module Eq = List.Eq (Int.Eq)
    module Show = List.Show (Int.Show)

    module Additive = struct
      module Fold_Map = List.Foldable.Fold_Map (Int.Additive.Monoid)
    end

    module Multiplicative = struct
      module Fold_Map = List.Foldable.Fold_Map (Int.Multiplicative.Monoid)
    end

    module Functions = struct
      module Scan = Functions.Travsersable.Scan (struct
        type t = int
      end)
    end
  end

  module Float = struct
    module Eq = List.Eq (Float.Eq)
    module Show = List.Show (Float.Show)

    module Additive = struct
      module Fold_Map = List.Foldable.Fold_Map (Float.Additive.Monoid)
    end

    module Multiplicative = struct
      module Fold_Map = List.Foldable.Fold_Map (Float.Multiplicative.Monoid)
    end

    module Functions = struct
      module Scan = Functions.Travsersable.Scan (struct
        type t = float
      end)
    end
  end

  module Bool = struct
    module Eq = List.Eq (Bool.Eq)
    module Show = List.Show (Bool.Show)
  end

  module String = struct
    module Eq = List.Eq (String.Eq)
  end

  module Array = struct
    module Traversable = List.Traversable (Array.Applicative)
  end

  module Option = struct
    module Fold_Map_Plus = List.Foldable.Fold_Map_Plus (Option.Plus)
    module Traversable = List.Traversable (Option.Applicative)
  end

  module Infix = struct
    module Functor = Infix.Functor (List.Functor)
    module Apply = Infix.Apply (List.Apply)
    module Monad = Infix.Monad (List.Monad)
    module Alt = Infix.Alt (List.Alt)
  end

  module List = struct
    module Fold_Map_Plus = List.Foldable.Fold_Map_Plus (List.Plus)
    module Traversable = List.Traversable (List.Applicative)
  end
end

module OptionF = struct
  module Int = struct
    module Eq = Option.Eq (Int.Eq)
    module Ord = Option.Ord (Int.Ord)

    module Additive = struct
      module Semigroup = Option.Semigroup (Int.Additive.Semigroup)
      module Quasigroup = Option.Quasigroup (Int.Additive.Quasigroup)
      module Monoid = Option.Monoid (Int.Additive.Semigroup)
      module Fold_Map = Option.Foldable.Fold_Map (Int.Additive.Monoid)
    end

    module Multiplicative = struct
      module Semigroup = Option.Semigroup (Int.Multiplicative.Semigroup)
      module Quasigroup = Option.Quasigroup (Int.Multiplicative.Quasigroup)
      module Monoid = Option.Monoid (Int.Multiplicative.Semigroup)
      module Fold_Map = Option.Foldable.Fold_Map (Int.Multiplicative.Monoid)
    end

    module Subtractive = struct
      module Quasigroup = Option.Quasigroup (Int.Subtractive.Quasigroup)
    end
  end

  module Float = struct
    module Eq = Option.Eq (Float.Eq)
    module Ord = Option.Ord (Float.Ord)

    module Additive = struct
      module Semigroup = Option.Semigroup (Float.Additive.Semigroup)
      module Quasigroup = Option.Quasigroup (Float.Additive.Quasigroup)
      module Monoid = Option.Monoid (Float.Additive.Semigroup)
      module Fold_Map = Option.Foldable.Fold_Map (Float.Additive.Monoid)
    end

    module Multiplicative = struct
      module Semigroup = Option.Semigroup (Float.Multiplicative.Semigroup)
      module Quasigroup = Option.Quasigroup (Float.Multiplicative.Quasigroup)
      module Monoid = Option.Monoid (Float.Multiplicative.Semigroup)
      module Fold_Map = Option.Foldable.Fold_Map (Float.Multiplicative.Monoid)
    end

    module Subtractive = struct
      module Quasigroup = Option.Quasigroup (Float.Subtractive.Quasigroup)
    end

    module Divisive = struct
      module Quasigroup = Option.Quasigroup (Float.Divisive.Quasigroup)
    end
  end

  module Bool = struct
    module Eq = Option.Eq (Bool.Eq)
    module Ord = Option.Ord (Bool.Ord)

    module Conjunctive = struct
      module Semigroup = Option.Semigroup (Bool.Conjunctive.Semigroup)
      module Monoid = Option.Monoid (Bool.Conjunctive.Semigroup)
    end

    module Disjunctive = struct
      module Semigroup = Option.Semigroup (Bool.Disjunctive.Semigroup)
      module Monoid = Option.Monoid (Bool.Disjunctive.Semigroup)
    end
  end

  module String = struct
    module Eq = Option.Eq (String.Eq)
    module Ord = Option.Ord (String.Ord)
    module Semigroup = Option.Semigroup (String.Semigroup)
    module Monoid = Option.Monoid (String.Semigroup)
  end

  module List = struct
    module Fold_Map_Plus = Option.Foldable.Fold_Map_Plus (List.Plus)
    module Traversable = Option.Traversable (List.Applicative)
  end

  module Array = struct
    module Traversable = Option.Traversable (Array.Applicative)
  end

  module Infix = struct
    module Functor = Infix.Functor (Option.Functor)
    module Apply = Infix.Apply (Option.Apply)
    module Monad = Infix.Monad (Option.Monad)
    module Alt = Infix.Alt (Option.Alt)
  end

  module Option = struct
    module Fold_Map_Plus = Option.Foldable.Fold_Map_Plus (Option.Plus)
    module Traversable = Option.Traversable (Option.Applicative)
  end
end

module ResultF = struct
  module Int = struct
    module Functor = Result.Functor (struct
      type t = int
    end)

    module Apply = Result.Apply (struct
      type t = int
    end)

    module Applicative = Result.Applicative (struct
      type t = int
    end)

    module Monad = Result.Monad (struct
      type t = int
    end)

    module Extend = Result.Extend (struct
      type t = int
    end)

    module Alt = Result.Alt (struct
      type t = int
    end)

    module Foldable = Result.Foldable (struct
      type t = int
    end)

    module List = struct
      module Traversable =
        Result.Traversable
          (struct
            type t = int
          end)
          (List.Applicative)
    end

    module Array = struct
      module Traversable =
        Result.Traversable
          (struct
            type t = int
          end)
          (Array.Applicative)
    end

    module Option = struct
      module Traversable =
        Result.Traversable
          (struct
            type t = int
          end)
          (Option.Applicative)
    end

    module Bool = struct
      module Eq = Result.Eq (OcamlAbstractInt.Eq) (OcamlAbstractBool.Eq)
      module Ord = Result.Ord (OcamlAbstractInt.Ord) (OcamlAbstractBool.Ord)
      module Show = Result.Show (OcamlAbstractInt.Show) (OcamlAbstractBool.Show)
    end

    module Float = struct
      module Eq = Result.Eq (OcamlAbstractInt.Eq) (OcamlAbstractFloat.Eq)
      module Ord = Result.Ord (OcamlAbstractInt.Ord) (OcamlAbstractFloat.Ord)
      module Show = Result.Show (OcamlAbstractInt.Show) (OcamlAbstractFloat.Show)
    end

    module String = struct
      module Eq = Result.Eq (OcamlAbstractInt.Eq) (OcamlAbstractString.Eq)
      module Ord = Result.Ord (OcamlAbstractInt.Ord) (OcamlAbstractString.Ord)
      module Show = Result.Show (OcamlAbstractInt.Show) (OcamlAbstractString.Show)
    end
  end

  module Float = struct
    module Functor = Result.Functor (struct
      type t = float
    end)

    module Apply = Result.Apply (struct
      type t = float
    end)

    module Applicative = Result.Applicative (struct
      type t = float
    end)

    module Monad = Result.Monad (struct
      type t = float
    end)

    module Extend = Result.Extend (struct
      type t = float
    end)

    module Alt = Result.Alt (struct
      type t = float
    end)

    module Foldable = Result.Foldable (struct
      type t = float
    end)

    module List = struct
      module Traversable =
        Result.Traversable
          (struct
            type t = float
          end)
          (List.Applicative)
    end

    module Array = struct
      module Traversable =
        Result.Traversable
          (struct
            type t = float
          end)
          (Array.Applicative)
    end

    module Option = struct
      module Traversable =
        Result.Traversable
          (struct
            type t = float
          end)
          (Option.Applicative)
    end

    module Bool = struct
      module Eq = Result.Eq (OcamlAbstractFloat.Eq) (OcamlAbstractBool.Eq)
      module Ord = Result.Ord (OcamlAbstractFloat.Ord) (OcamlAbstractBool.Ord)
      module Show = Result.Show (OcamlAbstractFloat.Show) (OcamlAbstractBool.Show)
    end

    module Int = struct
      module Eq = Result.Eq (OcamlAbstractFloat.Eq) (OcamlAbstractInt.Eq)
      module Ord = Result.Ord (OcamlAbstractFloat.Ord) (OcamlAbstractInt.Ord)
      module Show = Result.Show (OcamlAbstractFloat.Show) (OcamlAbstractInt.Show)
    end

    module String = struct
      module Eq = Result.Eq (OcamlAbstractFloat.Eq) (OcamlAbstractString.Eq)
      module Ord = Result.Ord (OcamlAbstractFloat.Ord) (OcamlAbstractString.Ord)
      module Show = Result.Show (OcamlAbstractFloat.Show) (OcamlAbstractString.Show)
    end
  end

  module Bool = struct
    module Functor = Result.Functor (struct
      type t = bool
    end)

    module Apply = Result.Apply (struct
      type t = bool
    end)

    module Applicative = Result.Applicative (struct
      type t = bool
    end)

    module Monad = Result.Monad (struct
      type t = bool
    end)

    module Extend = Result.Extend (struct
      type t = bool
    end)

    module Alt = Result.Alt (struct
      type t = bool
    end)

    module Foldable = Result.Foldable (struct
      type t = bool
    end)

    module List = struct
      module Traversable =
        Result.Traversable
          (struct
            type t = bool
          end)
          (List.Applicative)
    end

    module Array = struct
      module Traversable =
        Result.Traversable
          (struct
            type t = bool
          end)
          (Array.Applicative)
    end

    module Option = struct
      module Traversable =
        Result.Traversable
          (struct
            type t = bool
          end)
          (Option.Applicative)
    end

    module Int = struct
      module Eq = Result.Eq (OcamlAbstractBool.Eq) (OcamlAbstractInt.Eq)
      module Ord = Result.Ord (OcamlAbstractBool.Ord) (OcamlAbstractInt.Ord)
      module Show = Result.Show (OcamlAbstractBool.Show) (OcamlAbstractInt.Show)
    end

    module Float = struct
      module Eq = Result.Eq (OcamlAbstractBool.Eq) (OcamlAbstractFloat.Eq)
      module Ord = Result.Ord (OcamlAbstractBool.Ord) (OcamlAbstractFloat.Ord)
      module Show = Result.Show (OcamlAbstractBool.Show) (OcamlAbstractFloat.Show)
    end

    module String = struct
      module Eq = Result.Eq (OcamlAbstractBool.Eq) (OcamlAbstractString.Eq)
      module Ord = Result.Ord (OcamlAbstractBool.Ord) (OcamlAbstractString.Ord)
      module Show = Result.Show (OcamlAbstractBool.Show) (OcamlAbstractString.Show)
    end
  end

  module String = struct
    module Functor = Result.Functor (struct
      type t = string
    end)

    module Apply = Result.Apply (struct
      type t = string
    end)

    module Applicative = Result.Applicative (struct
      type t = string
    end)

    module Monad = Result.Monad (struct
      type t = string
    end)

    module Extend = Result.Extend (struct
      type t = string
    end)

    module Alt = Result.Alt (struct
      type t = string
    end)

    module Foldable = Result.Foldable (struct
      type t = string
    end)

    module List = struct
      module Traversable =
        Result.Traversable
          (struct
            type t = string
          end)
          (List.Applicative)
    end

    module Array = struct
      module Traversable =
        Result.Traversable
          (struct
            type t = string
          end)
          (Array.Applicative)
    end

    module Option = struct
      module Traversable =
        Result.Traversable
          (struct
            type t = string
          end)
          (Option.Applicative)
    end

    module Int = struct
      module Eq = Result.Eq (OcamlAbstractString.Eq) (OcamlAbstractInt.Eq)
      module Ord = Result.Ord (OcamlAbstractString.Ord) (OcamlAbstractInt.Ord)
      module Show = Result.Show (OcamlAbstractString.Show) (OcamlAbstractInt.Show)
    end

    module Float = struct
      module Eq = Result.Eq (OcamlAbstractString.Eq) (OcamlAbstractFloat.Eq)
      module Ord = Result.Ord (OcamlAbstractString.Ord) (OcamlAbstractFloat.Ord)
      module Show = Result.Show (OcamlAbstractString.Show) (OcamlAbstractFloat.Show)
    end

    module Bool = struct
      module Eq = Result.Eq (OcamlAbstractString.Eq) (OcamlAbstractBool.Eq)
      module Ord = Result.Ord (OcamlAbstractString.Ord) (OcamlAbstractBool.Ord)
      module Show = Result.Show (OcamlAbstractString.Show) (OcamlAbstractBool.Show)
    end
  end
end

module TupleF = struct
  module Int = struct
    module Functor = Tuple.Functor (struct
      type t = int
    end)

    module Foldable = Tuple.Foldable (struct
      type t = int
    end)

    module Additive = struct
      module Apply = Tuple.Apply (Int.Additive.Semigroup)
      module Applicative = Tuple.Applicative (Int.Additive.Monoid)
      module Monad = Tuple.Monad (Int.Additive.Monoid)
    end

    module Multiplicative = struct
      module Apply = Tuple.Apply (Int.Multiplicative.Semigroup)
      module Applicative = Tuple.Applicative (Int.Multiplicative.Monoid)
      module Monad = Tuple.Monad (Int.Multiplicative.Monoid)
    end

    module List = struct
      module Traversable =
        Tuple.Traversable
          (struct
            type t = int
          end)
          (List.Applicative)
    end

    module Array = struct
      module Traversable =
        Tuple.Traversable
          (struct
            type t = int
          end)
          (Array.Applicative)
    end

    module Option = struct
      module Traversable =
        Tuple.Traversable
          (struct
            type t = int
          end)
          (Option.Applicative)
    end
  end

  module Bool = struct
    module Functor = Tuple.Functor (struct
      type t = bool
    end)

    module Foldable = Tuple.Foldable (struct
      type t = bool
    end)

    module Conjunctive = struct
      module Apply = Tuple.Apply (Bool.Conjunctive.Semigroup)
      module Applicative = Tuple.Applicative (Bool.Conjunctive.Monoid)
      module Monad = Tuple.Monad (Bool.Conjunctive.Monoid)
    end

    module Disjunctive = struct
      module Apply = Tuple.Apply (Bool.Disjunctive.Semigroup)
      module Applicative = Tuple.Applicative (Bool.Disjunctive.Monoid)
      module Monad = Tuple.Monad (Bool.Disjunctive.Monoid)
    end

    module List = struct
      module Traversable =
        Tuple.Traversable
          (struct
            type t = bool
          end)
          (List.Applicative)
    end

    module Array = struct
      module Traversable =
        Tuple.Traversable
          (struct
            type t = bool
          end)
          (Array.Applicative)
    end

    module Option = struct
      module Traversable =
        Tuple.Traversable
          (struct
            type t = bool
          end)
          (Option.Applicative)
    end
  end

  module String = struct
    module Functor = Tuple.Functor (struct
      type t = string
    end)

    module Foldable = Tuple.Foldable (struct
      type t = string
    end)

    module Apply = Tuple.Apply (String.Semigroup)
    module Applicative = Tuple.Applicative (String.Monoid)
    module Monad = Tuple.Monad (String.Monoid)

    module List = struct
      module Traversable =
        Tuple.Traversable
          (struct
            type t = string
          end)
          (List.Applicative)
    end

    module Array = struct
      module Traversable =
        Tuple.Traversable
          (struct
            type t = string
          end)
          (Array.Applicative)
    end

    module Option = struct
      module Traversable =
        Tuple.Traversable
          (struct
            type t = string
          end)
          (Option.Applicative)
    end
  end
end

module FunctionF = struct
  module Int = struct
    module Functor = Function.Functor (struct
      type t = int
    end)
  end

  module Bool = struct
    module Functor = Function.Functor (struct
      type t = bool
    end)
  end

  module String = struct
    module Functor = Function.Functor (struct
      type t = string
    end)
  end
end
