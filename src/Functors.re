/* * * * * * * * * * * * * * * * *
 * Common instantiated functors  *
 * * * * * * * * * * * * * * * * */
module ArrayF = {
  module Functions = {
    module Travsersable = Functions.Traversable(Array.Traversable);
  };

  module Int = {
    module Eq = Array.Eq(Int.Eq);
    module Additive = {
      module Fold_Map = Array.Foldable.Fold_Map(Int.Additive.Monoid);
    };
    module Multiplicative = {
      module Fold_Map = Array.Foldable.Fold_Map(Int.Multiplicative.Monoid);
    };
    module Functions = {
      module Scan = Functions.Travsersable.Scan({type s = int});
    };
  };

  module Bool = {
    module Eq = Array.Eq(Bool.Eq);
  };

  module String = {
    module Eq = Array.Eq(String.Eq);
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
    module Additive = {
      module Fold_Map = List.Foldable.Fold_Map(Int.Additive.Monoid);
    };
    module Multiplicative = {
      module Fold_Map = List.Foldable.Fold_Map(Int.Multiplicative.Monoid);
    };
    module Functions = {
      module Scan = Functions.Travsersable.Scan({type s = int});
    };
  };

  module Bool = {
    module Eq = List.Eq(Bool.Eq);
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
