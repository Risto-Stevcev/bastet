open BsMocha.Mocha
open BsJsverify.Verify.Arbitrary
open BsJsverify.Verify.Property

let ( <. ) = Function.Infix.( <. )

;;
describe "Date" (fun () ->
    describe "Medial Magma" (fun () ->
        let module V = Verify.Medial_Magma (Date.Medial_Magma) in
        property4
          "should satisfy bicommutativity"
          arb_date
          arb_date
          arb_date
          arb_date
          V.bicommutativity);
    describe "Semigroup" (fun () ->
        let module V = Verify.Semigroup (Date.Semigroup) in
        property3 "should satisfy associativity" arb_date arb_date arb_date V.associativity);
    describe "Monoid" (fun () ->
        let module V = Verify.Monoid (Date.Monoid) in
        property1 "should satisfy identity" arb_date V.identity);
    describe "Eq" (fun () ->
        let module V = Verify.Eq (Date.Eq) in
        property1 "should satisfy reflexivity" arb_date V.reflexivity;
        property2 "should satisfy symmetry" arb_date arb_date V.symmetry;
        property3 "should satisfy transitivity" arb_date arb_date arb_date V.transitivity);
    describe "Ord" (fun () ->
        let module V = Verify.Ord (Date.Ord) in
        property1 "should satisfy reflexivity" arb_date V.reflexivity;
        property2 "should satisfy antisymmetry" arb_date arb_date V.antisymmetry;
        property3 "should satisfy transitivity" arb_date arb_date arb_date V.transitivity))
