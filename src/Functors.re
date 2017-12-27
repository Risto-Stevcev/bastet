/* * * * * * * * * * * * * * * * *
 * Common instantiated functors  *
 * * * * * * * * * * * * * * * * */
module ArrayF = {
  module Functions = {
    module Travsersable = Functions.Traversable(Array.Traversable);
  };
  module Int = {
    module Eq = Array.Eq(Int.Eq);
    module Ord = Array.Ord(Int.Ord);
    module Show = Array.Show(Int.Show);
    module Additive = {
      module Fold_Map = Array.Foldable.Fold_Map(Int.Additive.Monoid);
    };
    module Multiplicative = {
      module Fold_Map = Array.Foldable.Fold_Map(Int.Multiplicative.Monoid);
    };
    module Functions = {
      module Scan = Functions.Travsersable.Scan({type t = int});
    };
  };
  module Float = {
    module Eq = Array.Eq(Float.Eq);
    module Ord = Array.Ord(Float.Ord);
    module Show = Array.Show(Float.Show);
    module Additive = {
      module Fold_Map = Array.Foldable.Fold_Map(Float.Additive.Monoid);
    };
    module Multiplicative = {
      module Fold_Map = Array.Foldable.Fold_Map(Float.Multiplicative.Monoid);
    };
    module Functions = {
      module Scan = Functions.Travsersable.Scan({type t = float});
    };
  };
  module Bool = {
    module Eq = Array.Eq(Bool.Eq);
    module Ord = Array.Ord(Bool.Ord);
    module Show = Array.Show(Bool.Show);
  };
  module String = {
    module Eq = Array.Eq(String.Eq);
    module Ord = Array.Ord(String.Ord);
  };
  module List = {
    module Fold_Map_Plus = Array.Foldable.Fold_Map_Plus(List.Plus);
    module Traversable = Array.Traversable(List.Applicative);
  };
  module Option = {
    module Fold_Map_Plus = Array.Foldable.Fold_Map_Plus(Option.Plus);
    module Traversable = Array.Traversable(Option.Applicative);
  };
  module Infix = {
    module Functor = Infix.Functor(Array.Functor);
    module Apply = Infix.Apply(Array.Apply);
    module Monad = Infix.Monad(Array.Monad);
    module Alt = Infix.Alt(Array.Alt);
  };
  module Array = {
    module Fold_Map_Plus = Array.Foldable.Fold_Map_Plus(Array.Plus);
    module Traversable = Array.Traversable(Array.Applicative);
  };
};



module ListF = {
  module Functions = {
    module Travsersable = Functions.Traversable(List.Traversable);
  };
  module Int = {
    module Eq = List.Eq(Int.Eq);
    module Show = List.Show(Int.Show);
    module Additive = {
      module Fold_Map = List.Foldable.Fold_Map(Int.Additive.Monoid);
    };
    module Multiplicative = {
      module Fold_Map = List.Foldable.Fold_Map(Int.Multiplicative.Monoid);
    };
    module Functions = {
      module Scan = Functions.Travsersable.Scan({type t = int});
    };
  };
  module Float = {
    module Eq = List.Eq(Float.Eq);
    module Show = List.Show(Float.Show);
    module Additive = {
      module Fold_Map = List.Foldable.Fold_Map(Float.Additive.Monoid);
    };
    module Multiplicative = {
      module Fold_Map = List.Foldable.Fold_Map(Float.Multiplicative.Monoid);
    };
    module Functions = {
      module Scan = Functions.Travsersable.Scan({type t = float});
    };
  };
  module Bool = {
    module Eq = List.Eq(Bool.Eq);
    module Show = List.Show(Bool.Show);
  };
  module String = {
    module Eq = List.Eq(String.Eq);
  };
  module Array = {
    module Fold_Map_Plus = List.Foldable.Fold_Map_Plus(Array.Plus);
    module Traversable = List.Traversable(Array.Applicative);
  };
  module Option = {
    module Fold_Map_Plus = List.Foldable.Fold_Map_Plus(Option.Plus);
    module Traversable = List.Traversable(Option.Applicative);
  };
  module Infix = {
    module Functor = Infix.Functor(List.Functor);
    module Apply = Infix.Apply(List.Apply);
    module Monad = Infix.Monad(List.Monad);
    module Alt = Infix.Alt(List.Alt);
  };
  module List = {
    module Fold_Map_Plus = List.Foldable.Fold_Map_Plus(List.Plus);
    module Traversable = List.Traversable(List.Applicative);
  };
};



module OptionF = {
  module Int = {
    module Eq = Option.Eq(Int.Eq);
    module Additive = {
      module Semigroup = Option.Semigroup(Int.Additive.Semigroup);
      module Monoid = Option.Monoid(Int.Additive.Semigroup);
      module Fold_Map = Option.Foldable.Fold_Map(Int.Additive.Monoid);
    };
    module Multiplicative = {
      module Semigroup = Option.Semigroup(Int.Multiplicative.Semigroup);
      module Monoid = Option.Monoid(Int.Multiplicative.Semigroup);
      module Fold_Map = Option.Foldable.Fold_Map(Int.Multiplicative.Monoid);
    };
  };
  module Float = {
    module Eq = Option.Eq(Float.Eq);
    module Additive = {
      module Semigroup = Option.Semigroup(Float.Additive.Semigroup);
      module Monoid = Option.Monoid(Float.Additive.Semigroup);
      module Fold_Map = Option.Foldable.Fold_Map(Float.Additive.Monoid);
    };
    module Multiplicative = {
      module Semigroup = Option.Semigroup(Float.Multiplicative.Semigroup);
      module Monoid = Option.Monoid(Float.Multiplicative.Semigroup);
      module Fold_Map = Option.Foldable.Fold_Map(Float.Multiplicative.Monoid);
    };
  };
  module Bool = {
    module Eq = Option.Eq(Bool.Eq);
    module Conjunctive = {
      module Semigroup = Option.Semigroup(Bool.Conjunctive.Semigroup);
      module Monoid = Option.Monoid(Bool.Conjunctive.Semigroup);
    };
    module Disjunctive = {
      module Semigroup = Option.Semigroup(Bool.Disjunctive.Semigroup);
      module Monoid = Option.Monoid(Bool.Disjunctive.Semigroup);
    };
  };
  module String = {
    module Eq = Option.Eq(String.Eq);
    module Semigroup = Option.Semigroup(String.Semigroup);
    module Monoid = Option.Monoid(String.Semigroup);
  };
  module List = {
    module Fold_Map_Plus = Option.Foldable.Fold_Map_Plus(List.Plus);
    module Traversable = Option.Traversable(List.Applicative);
  };
  module Array = {
    module Fold_Map_Plus = Option.Foldable.Fold_Map_Plus(Array.Plus);
    module Traversable = Option.Traversable(Array.Applicative);
  };
  module Infix = {
    module Functor = Infix.Functor(Option.Functor);
    module Apply = Infix.Apply(Option.Apply);
    module Monad = Infix.Monad(Option.Monad);
    module Alt = Infix.Alt(Option.Alt);
  };
  module Option = {
    module Fold_Map_Plus = Option.Foldable.Fold_Map_Plus(Option.Plus);
    module Traversable = Option.Traversable(Option.Applicative);
  };
};


module TupleF = {
  module Int = {
    module Functor = Tuple.Functor({type t = int});
    module Foldable = Tuple.Foldable({type t = int});
    module Additive = {
      module Apply = Tuple.Apply(Int.Additive.Semigroup);
      module Applicative = Tuple.Applicative(Int.Additive.Monoid);
      module Monad = Tuple.Monad(Int.Additive.Monoid);
    };
    module Multiplicative = {
      module Apply = Tuple.Apply(Int.Multiplicative.Semigroup);
      module Applicative = Tuple.Applicative(Int.Multiplicative.Monoid);
      module Monad = Tuple.Monad(Int.Multiplicative.Monoid);
    };
    module List = {
      module Traversable = Tuple.Traversable({type t = int}, List.Applicative);
    };
    module Array = {
      module Traversable = Tuple.Traversable({type t = int}, Array.Applicative);
    };
    module Option = {
      module Traversable = Tuple.Traversable({type t = int}, Option.Applicative);
    };
  };
  module Bool = {
    module Functor = Tuple.Functor({type t = bool});
    module Foldable = Tuple.Foldable({type t = bool});
    module Conjunctive = {
      module Apply = Tuple.Apply(Bool.Conjunctive.Semigroup);
      module Applicative = Tuple.Applicative(Bool.Conjunctive.Monoid);
      module Monad = Tuple.Monad(Bool.Conjunctive.Monoid);
    };
    module Disjunctive = {
      module Apply = Tuple.Apply(Bool.Disjunctive.Semigroup);
      module Applicative = Tuple.Applicative(Bool.Disjunctive.Monoid);
      module Monad = Tuple.Monad(Bool.Disjunctive.Monoid);
    };
    module List = {
      module Traversable = Tuple.Traversable({type t = bool}, List.Applicative);
    };
    module Array = {
      module Traversable = Tuple.Traversable({type t = bool}, Array.Applicative);
    };
    module Option = {
      module Traversable = Tuple.Traversable({type t = bool}, Option.Applicative);
    };
  };
  module String = {
    module Functor = Tuple.Functor({type t = string});
    module Foldable = Tuple.Foldable({type t = string});
    module Apply = Tuple.Apply(String.Semigroup);
    module Applicative = Tuple.Applicative(String.Monoid);
    module Monad = Tuple.Monad(String.Monoid);
    module List = {
      module Traversable = Tuple.Traversable({type t = string}, List.Applicative);
    };
    module Array = {
      module Traversable = Tuple.Traversable({type t = string}, Array.Applicative);
    };
    module Option = {
      module Traversable = Tuple.Traversable({type t = string}, Option.Applicative);
    };
  };
};


module FunctionF = {
  module Int = {
    module Functor = Function.Functor({type t = int});
  };
  module Bool = {
    module Functor = Function.Functor({type t = bool});
  };
  module String = {
    module Functor = Function.Functor({type t = string});
  };
};
