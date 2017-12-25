open Mocha;
module Fn = Infix.Semigroupoid(Function.Semigroupoid);


describe("Functions", () => {
  describe("Traversable", () => {
    module List_Traversable = Functions.Traversable(List.Traversable);
    module Array_Traversable  = Functions.Traversable(Array.Traversable);

    describe("Scan", () => {
      module List_Scan = List_Traversable.Scan({type s = int});
      module Array_Scan = Array_Traversable.Scan({type s = int});

      describe("scan_left", () => {
        it("should scan from the left (list)", () => List_Scan.({
          expect(scan_left((+), 0, [1,2,3])).to_be([1,3,6]);
          expect(scan_left((-), 10, [1,2,3])).to_be([9,7,4]);
        }));

        it("should scan from the left (array)", () => Array_Scan.({
          expect(scan_left((+), 0, [|1,2,3|])).to_be([|1,3,6|]);
          expect(scan_left((-), 10, [|1,2,3|])).to_be([|9,7,4|]);
        }));
      });

      describe("scan_right", () => {
        it("should scan from the right (list)", () => List_Scan.({
          expect(scan_right((+), 0, [1,2,3])).to_be([6,5,3]);
          expect(scan_right(Function.flip((-)), 10, [1,2,3])).to_be([4,5,7]);
        }));

        it("should scan from the right (array)", () => Array_Scan.({
          expect(scan_right((+), 0, [|1,2,3|])).to_be([|6,5,3|]);
          expect(scan_right(Function.flip((-)), 10, [|1,2,3|])).to_be([|4,5,7|]);
        }));
      });
    });
  });
});
