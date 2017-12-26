open Mocha;
open Interface;


describe("Default", () => {
  module Foldable: FOLDABLE with type t('a) = list('a) = {
    type t('a) = list('a);

    module FM: Default.FOLD_MAP with type t('a) = list('a) = {
      type t('a) = list('a);
      module Fold_Map_Any = (M: MONOID_ANY) => {
        let fold_map = (f, x) =>
          ListLabels.fold_left(~f=(acc, x) => M.append(acc, f(x)), ~init=M.empty, x);
      };
      module Fold_Map_Plus = (P: PLUS) => {
        let fold_map = (f, x) =>
          ListLabels.fold_left(~f=(acc, x) => P.alt(acc, f(x)), ~init=P.empty, x);
      };
    };
    module Fold_Map = List.Foldable.Fold_Map;
    module Fold_Map_Any = FM.Fold_Map_Any;
    module Fold_Map_Plus = FM.Fold_Map_Plus;
    module F = Default.Fold(FM);

    let (fold_left, fold_right) = (F.fold_left_default, F.fold_right_default);
  };

  module Traversable = (A: APPLICATIVE) => {
    module List_Traversable: TRAVERSABLE
      with type applicative_t('a) = A.t('a) and type t('a) = list('a) = {

      type t('a) = list('a);
      type applicative_t('a) = A.t('a);
      include (List.Functor: FUNCTOR with type t('a) := t('a));
      include (List.Foldable: FOLDABLE with type t('a) := t('a));

      module I = Infix.Apply(A);
      let sequence = (xs) => I.({
        ListLabels.fold_right(
          ~f=(acc, x) => A.pure((y, ys) => [y, ...ys]) <*> acc <*> x,
          ~init=A.pure([]),
          xs
        )
      });

      module D = Default.Traverse({
        type t('a) = list('a);
        type applicative_t('a) = A.t('a);
        include (List.Functor: FUNCTOR with type t('a) := t('a));
        let sequence = sequence;
      });
      let traverse = D.traverse_default;
    };
    include List_Traversable
  };

  describe("Foldable", () => Foldable.({
    it("should do a left fold", () => {
      expect(fold_left((+), 0, [1,2,3,4,5])).to_be(15);
      expect(fold_left((-), 10, [3,2,1])).to_be(4);
    });
    it("should do a right fold", () => {
      expect(fold_right((-), 10, [3,2,1])).to_be(-8);
    });
  }));

  describe("Traversable", () => {
    module T = Traversable(Option.Applicative);
    it("should traverse the list", () => T.({
      let positive_int = (x) => x >= 0 ? Some(x) : None;
      expect(traverse(positive_int, [1,2,3])).to_be(Some([1,2,3]));
      expect(traverse(positive_int, [1,2,-3])).to_be(None);
    }));
    it("should sequence the list", () => T.({
      expect(sequence([Some(3), Some(4), Some(5)])).to_be(Some([3,4,5]));
      expect(sequence([Some(3), Some(4), None])).to_be(None);
    }));
  });
});
