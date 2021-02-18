open BsMocha.Mocha
open BsChai.Expect.Expect
open BsChai.Expect.Combos.End
open BsJsverify.Verify.Arbitrary
open BsJsverify.Verify.Property
open Belt.Result

type 'a arbitrary = 'a BsJsverify.Verify.Arbitrary.arbitrary

let ( <. ), ( >. ) =
  let open Function.Infix in
  ( <. ), ( >. )

let const, id = Function.const, Function.Category.id

module Toggle = struct
  open Interface

  type toggle =
    | Off
    | On

  let to_bool x =
    match x with
    | On -> true
    | Off -> false

  let from_bool x =
    match x with
    | true -> On
    | false -> Off

  let and_ x y =
    match x, y with
    | On, On -> On
    | _ -> Off

  let or_ x y =
    match x, y with
    | Off, Off -> Off
    | _ -> On

  let not__ x =
    match x with
    | On -> Off
    | Off -> On

  module Eq : EQ with type t = toggle = struct
    type t = toggle

    let eq = ( = )
  end

  module Ord : ORD with type t = toggle = struct
    include Eq

    let compare = unsafe_compare
  end

  module Bounded : BOUNDED with type t = toggle = struct
    include Ord

    let top = On

    and bottom = Off
  end

  module Join_Semilattice : JOIN_SEMILATTICE with type t = toggle = struct
    type t = toggle

    let join = or_
  end

  module Meet_Semilattice : MEET_SEMILATTICE with type t = toggle = struct
    type t = toggle

    let meet = and_
  end

  module Bounded_Join_Semilattice : BOUNDED_JOIN_SEMILATTICE with type t = toggle = struct
    include Join_Semilattice

    let bottom = Off
  end

  module Bounded_Meet_Semilattice : BOUNDED_MEET_SEMILATTICE with type t = toggle = struct
    include Meet_Semilattice

    let top = On
  end

  module Lattice : LATTICE with type t = toggle = struct
    include Join_Semilattice

    include (Meet_Semilattice : MEET_SEMILATTICE with type t := t)
  end

  module Bounded_Lattice : BOUNDED_LATTICE with type t = toggle = struct
    include Bounded_Join_Semilattice

    include (Bounded_Meet_Semilattice : BOUNDED_MEET_SEMILATTICE with type t := t)
  end

  module Distributive_Lattice : DISTRIBUTIVE_LATTICE with type t = toggle = struct
    include Lattice
  end

  module Bounded_Distributive_Lattice : BOUNDED_DISTRIBUTIVE_LATTICE with type t = toggle = struct
    include Bounded_Lattice
  end

  module Heyting_Algebra : HEYTING_ALGEBRA with type t = toggle = struct
    include Ord

    include (Bounded_Distributive_Lattice : BOUNDED_DISTRIBUTIVE_LATTICE with type t := t)

    let not = not__

    and implies a b = or_ (not__ a) b
  end

  module Involutive_Heyting_Algebra : INVOLUTIVE_HEYTING_ALGEBRA with type t = toggle = struct
    include Heyting_Algebra
  end

  module Boolean_Algebra : BOOLEAN_ALGEBRA with type t = toggle = struct
    include Heyting_Algebra
  end

  let arb_toggle =
    (smap
       from_bool
       to_bool
       ~newShow:(to_bool >. Js.Json.stringifyAny >. Js.Option.getWithDefault "")
       arb_bool
      : toggle arbitrary)

  ;;
  describe "Toggle" (fun () ->
      describe "Join_Semilattice" (fun () ->
          let module V = Verify.Join_Semilattice (Join_Semilattice) in
          property3 "should satisfy associativity" arb_toggle arb_toggle arb_toggle V.associativity;
          property2 "should satisfy commutativity" arb_toggle arb_toggle V.commutativity;
          property1 "should satisfy idempotency" arb_toggle V.idempotency);
      describe "Meet_Semilattice" (fun () ->
          let module V = Verify.Meet_Semilattice (Meet_Semilattice) in
          property3 "should satisfy associativity" arb_toggle arb_toggle arb_toggle V.associativity;
          property2 "should satisfy commutativity" arb_toggle arb_toggle V.commutativity;
          property1 "should satisfy idempotency" arb_toggle V.idempotency);
      describe "Bounded_Join_Semilattice" (fun () ->
          let module V = Verify.Bounded_Join_Semilattice (Bounded_Join_Semilattice) in
          property1 "should satisfy identity" arb_toggle V.identity);
      describe "Bounded_Meet_Semilattice" (fun () ->
          let module V = Verify.Bounded_Meet_Semilattice (Bounded_Meet_Semilattice) in
          property1 "should satisfy identity" arb_toggle V.identity);
      describe "Lattice" (fun () ->
          let module V = Verify.Lattice (Lattice) in
          property2 "should satisfy absorption" arb_toggle arb_toggle V.absorption);
      describe "Bounded_Lattice" (fun () ->
          let module V = Verify.Bounded_Lattice (Bounded_Lattice) in
          property2 "should satisfy absorption" arb_toggle arb_toggle V.absorption);
      describe "Distributive_Lattice" (fun () ->
          let module V = Verify.Distributive_Lattice (Distributive_Lattice) in
          property3
            "should satisfy distributivity"
            arb_toggle
            arb_toggle
            arb_toggle
            V.distributivity);
      describe "Bounded_Distributive_Lattice" (fun () ->
          let module V = Verify.Bounded_Distributive_Lattice (Bounded_Distributive_Lattice) in
          property3
            "should satisfy distributivity"
            arb_toggle
            arb_toggle
            arb_toggle
            V.distributivity);
      describe "Heyting_Algebra" (fun () ->
          let module V = Verify.Heyting_Algebra (Heyting_Algebra) in
          property1 "should satisfy pseudocomplement" arb_toggle V.pseudocomplement;
          property3
            "should satisfy relative pseudocomplement"
            arb_toggle
            arb_toggle
            arb_toggle
            V.relative_pseudocomplement);
      describe "Involutive_Heyting_Algebra" (fun () ->
          let module V = Verify.Involutive_Heyting_Algebra (Involutive_Heyting_Algebra) in
          property1 "should satisfy involution" arb_toggle V.involution);
      describe "Boolean_Algebra" (fun () ->
          let module V = Verify.Boolean_Algebra (Boolean_Algebra) in
          property1 "should satisfy the law of excluded middle" arb_toggle V.excluded_middle))
end

let arb_result =
  (fun arb_ok arb_error ->
     smap
       (fun e ->
         match e with
         | Types.Left l -> Error l
         | Types.Right r -> Ok r)
       (fun e ->
         match e with
         | Ok r -> Types.Right r
         | Error l -> Types.Left l)
       ~newShow:(fun a ->
         match a with
         | Ok a' -> "Ok(" ^ (Js.Json.stringifyAny a' |> Js.Option.getWithDefault "") ^ ")"
         | Error a' -> "Error(" ^ (Js.Json.stringifyAny a' |> Js.Option.getWithDefault "") ^ ")")
       (arb_either arb_error arb_ok)
    : 'a arbitrary -> 'b arbitrary -> ('a, 'b) Belt.Result.t arbitrary)

;;
describe "Result" (fun () ->
    describe "Medial Magma" (fun () ->
        let module Medial_Magma =
          Result.Medial_Magma
            (struct
              type t = string
            end)
            (Int.Additive.Medial_Magma)
        in
        let module V = Verify.Medial_Magma (Medial_Magma) in
        property4
          "should satisfy bicommutativity"
          (arb_result arb_nat arb_string)
          (arb_result arb_nat arb_string)
          (arb_result arb_nat arb_string)
          (arb_result arb_nat arb_string)
          V.bicommutativity);
    describe "Semigroup" (fun () ->
        let module Semigroup =
          Result.Semigroup
            (struct
              type t = string
            end)
            (Int.Additive.Semigroup)
        in
        let module V = Verify.Semigroup (Semigroup) in
        property3
          "should satisfy associativity"
          (arb_result arb_nat arb_string)
          (arb_result arb_nat arb_string)
          (arb_result arb_nat arb_string)
          V.associativity);
    describe "Functor" (fun () ->
        let module V = Verify.Functor (Functors.ResultF.String.Functor) in
        property1 "should satisfy identity" (arb_result arb_nat arb_string) V.identity;
        property1
          "should satisfy composition"
          (arb_result arb_nat arb_string)
          (V.composition (( ^ ) "!") string_of_int));
    describe "Bifunctor" (fun () ->
        let module V = Verify.Bifunctor (Result.Bifunctor) in
        property1 "should satisfy identity" (arb_result arb_string arb_nat) V.identity;
        property1
          "should satisfy composition"
          (arb_result arb_string arb_nat)
          (V.composition (( ^ ) "!") (( *. ) 3.0) (( ^ ) "-") float_of_int));
    describe "Apply" (fun () ->
        let module V = Verify.Apply (Functors.ResultF.String.Apply) in
        property1 "should satisfy associative composition" (arb_result arb_nat arb_string) (fun n ->
            V.associative_composition (Ok (( ^ ) "!")) (Ok string_of_int) n));
    describe "Applicative" (fun () ->
        let module V = Verify.Applicative (Functors.ResultF.String.Applicative) in
        property1 "should satisfy identity" (arb_result arb_nat arb_string) V.identity;
        property1
          "should satisfy homomorphism"
          (arb_result arb_nat arb_string)
          (V.homomorphism (Functors.ResultF.String.Functor.map string_of_int));
        property1 "should satisfy interchange" arb_nat (V.interchange (Ok string_of_int)));
    describe "Monad" (fun () ->
        let module V = Verify.Monad (Functors.ResultF.String.Monad) in
        let pure = Functors.ResultF.String.Applicative.pure in
        property1
          "should satisfy associativity"
          (arb_result arb_nat arb_string)
          (V.associativity (pure <. string_of_int) (pure <. ( ^ ) "!"));
        property1 "should satisfy identity" arb_nat (V.identity (pure <. string_of_int)));
    describe "Alt" (fun () ->
        let module V = Verify.Alt (Functors.ResultF.String.Alt) in
        property3
          "should satisfy associativity"
          (arb_result arb_nat arb_string)
          (arb_result arb_nat arb_string)
          (arb_result arb_nat arb_string)
          V.associativity;
        property2
          "should satisfy distributivity"
          (arb_result arb_nat arb_string)
          (arb_result arb_nat arb_string)
          (V.distributivity string_of_int));
    describe "Extend" (fun () ->
        let module V = Verify.Extend (Functors.ResultF.Bool.Extend) in
        property1
          "should satisfy associativity"
          (arb_result arb_nat arb_bool)
          (V.associativity
             (Result.result Js.Float.toString (const String.Monoid.empty))
             (Result.result float_of_int (const Float.Additive.Monoid.empty))));
    describe "Show" (fun () ->
        it "should show the either value" (fun () ->
            expect (Functors.ResultF.Bool.Int.Show.show (Ok true)) |> to_be "true";
            expect (Functors.ResultF.Bool.Int.Show.show (Error 123)) |> to_be "123"));
    describe "Eq" (fun () ->
        it "should compare two either values for equality" (fun () ->
            let module E = Result.Eq (Int.Eq) (Int.Eq) in
            let eq = Functors.ResultF.Float.Int.Eq.eq in
            expect (eq (Error 123) (Error 123)) |> to_be true;
            expect (eq (Error 123) (Error 456)) |> to_be false;
            expect (eq (Ok 12.3) (Ok 12.3)) |> to_be true;
            expect (eq (Ok 12.3) (Ok 45.6)) |> to_be false;
            expect (eq (Error 123) (Ok 45.6)) |> to_be false;
            expect (E.eq (Error 123) (Ok 123)) |> to_be false;
            expect (E.eq (Ok 123) (Error 123)) |> to_be false));
    describe "Ord" (fun () ->
        it "should compare two either values for equality" (fun () ->
            let module E = Result.Ord (Int.Ord) (Int.Ord) in
            let compare = Functors.ResultF.Float.Int.Ord.compare in
            expect (compare (Error 123) (Error 123)) |> to_be `equal_to;
            expect (compare (Error 123) (Error 456)) |> to_be `less_than;
            expect (compare (Ok 12.3) (Ok 12.3)) |> to_be `equal_to;
            expect (compare (Ok 12.3) (Ok 45.6)) |> to_be `less_than;
            expect (compare (Error 123) (Ok 45.6)) |> to_be `less_than;
            expect (E.compare (Error 123) (Ok 123)) |> to_be `less_than;
            expect (E.compare (Ok 123) (Error 123)) |> to_be `greater_than));
    describe "Bounded" (fun () ->
        let arb_float' = arb_float Float.Bounded.bottom Float.Bounded.top in
        let module V = Verify.Bounded (Result.Bounded (Int.Bounded) (Float.Bounded)) in
        property1 "should satisfy bounded" (arb_result arb_nat arb_float') V.bounded);
    describe "Join_Semilattice" (fun () ->
        let module V =
          Verify.Join_Semilattice
            (Result.Many_Valued_Logic.Join_Semilattice
               (Bool.Join_Semilattice)
               (Toggle.Join_Semilattice)) in
        property3
          "should satisfy associativity"
          (arb_result arb_bool Toggle.arb_toggle)
          (arb_result arb_bool Toggle.arb_toggle)
          (arb_result arb_bool Toggle.arb_toggle)
          V.associativity;
        property2
          "should satisfy commutativity"
          (arb_result arb_bool Toggle.arb_toggle)
          (arb_result arb_bool Toggle.arb_toggle)
          V.commutativity;
        property1 "should satisfy idempotency" (arb_result arb_bool Toggle.arb_toggle) V.idempotency);
    describe "Meet_Semilattice" (fun () ->
        let module V =
          Verify.Meet_Semilattice
            (Result.Many_Valued_Logic.Meet_Semilattice
               (Bool.Meet_Semilattice)
               (Toggle.Meet_Semilattice)) in
        property3
          "should satisfy associativity"
          (arb_result arb_bool Toggle.arb_toggle)
          (arb_result arb_bool Toggle.arb_toggle)
          (arb_result arb_bool Toggle.arb_toggle)
          V.associativity;
        property2
          "should satisfy commutativity"
          (arb_result arb_bool Toggle.arb_toggle)
          (arb_result arb_bool Toggle.arb_toggle)
          V.commutativity;
        property1 "should satisfy idempotency" (arb_result arb_bool Toggle.arb_toggle) V.idempotency);
    describe "Bounded_Join_Semilattice" (fun () ->
        let module V =
          Verify.Bounded_Join_Semilattice
            (Result.Many_Valued_Logic.Bounded_Join_Semilattice
               (Bool.Bounded_Join_Semilattice)
               (Toggle.Bounded_Join_Semilattice)) in
        property1 "should satisfy identity" (arb_result arb_bool Toggle.arb_toggle) V.identity);
    describe "Bounded_Meet_Semilattice" (fun () ->
        let module V =
          Verify.Bounded_Meet_Semilattice
            (Result.Many_Valued_Logic.Bounded_Meet_Semilattice
               (Bool.Bounded_Meet_Semilattice)
               (Toggle.Bounded_Meet_Semilattice)) in
        property1 "should satisfy identity" (arb_result arb_bool Toggle.arb_toggle) V.identity);
    describe "Lattice" (fun () ->
        let module V =
          Verify.Lattice (Result.Many_Valued_Logic.Lattice (Bool.Lattice) (Toggle.Lattice)) in
        property2
          "should satisfy absorption"
          (arb_result arb_bool Toggle.arb_toggle)
          (arb_result arb_bool Toggle.arb_toggle)
          V.absorption);
    describe "Bounded_Lattice" (fun () ->
        let module V =
          Verify.Bounded_Lattice
            (Result.Many_Valued_Logic.Bounded_Lattice
               (Bool.Bounded_Lattice)
               (Toggle.Bounded_Lattice)) in
        property2
          "should satisfy absorption"
          (arb_result arb_bool Toggle.arb_toggle)
          (arb_result arb_bool Toggle.arb_toggle)
          V.absorption);
    describe "Distributive_Lattice" (fun () ->
        let module V =
          Verify.Distributive_Lattice
            (Result.Many_Valued_Logic.Distributive_Lattice
               (Bool.Distributive_Lattice)
               (Toggle.Distributive_Lattice)) in
        property3
          "should satisfy distributivity"
          (arb_result arb_bool Toggle.arb_toggle)
          (arb_result arb_bool Toggle.arb_toggle)
          (arb_result arb_bool Toggle.arb_toggle)
          V.distributivity);
    describe "Bounded_Distributive_Lattice" (fun () ->
        let module V =
          Verify.Bounded_Distributive_Lattice
            (Result.Many_Valued_Logic.Bounded_Distributive_Lattice
               (Bool.Bounded_Distributive_Lattice)
               (Toggle.Bounded_Distributive_Lattice)) in
        property3
          "should satisfy distributivity"
          (arb_result arb_bool Toggle.arb_toggle)
          (arb_result arb_bool Toggle.arb_toggle)
          (arb_result arb_bool Toggle.arb_toggle)
          V.distributivity);
    describe "Heyting_Algebra" (fun () ->
        let module V =
          Verify.Heyting_Algebra
            (Result.Many_Valued_Logic.Heyting_Algebra
               (Bool.Heyting_Algebra)
               (Toggle.Heyting_Algebra)) in
        property1
          "should satisfy pseudocomplement"
          (arb_result arb_bool Toggle.arb_toggle)
          V.pseudocomplement;
        property3
          "should satisfy relative pseudocomplement"
          (arb_result arb_bool Toggle.arb_toggle)
          (arb_result arb_bool Toggle.arb_toggle)
          (arb_result arb_bool Toggle.arb_toggle)
          V.relative_pseudocomplement);
    describe "Involutive_Heyting_Algebra" (fun () ->
        let module V =
          Verify.Involutive_Heyting_Algebra
            (Result.Many_Valued_Logic.Involutive_Heyting_Algebra
               (Bool.Involutive_Heyting_Algebra)
               (Toggle.Involutive_Heyting_Algebra)) in
        property1 "should satisfy involution" (arb_result arb_bool Toggle.arb_toggle) V.involution);
    describe "Boolean_Algebra" (fun () ->
        let module V =
          Verify.Compare.Boolean_Algebra
            (Result.Many_Valued_Logic.Boolean_Algebra
               (Bool.Boolean_Algebra)
               (Toggle.Boolean_Algebra))
               (Result.Many_Valued_Logic.Quasireflexive_Eq
                  (Bool.Boolean_Algebra)
                  (Toggle.Boolean_Algebra))
        in
        property1
          "should satisfy the law of excluded middle"
          (arb_result arb_bool Toggle.arb_toggle)
          V.excluded_middle);
    describe "Result_Utilities" (fun () ->
        let errResult = (Belt.Result.Error "ERROR" : (int, string) Belt.Result.t) in
        let okResult = (Belt.Result.Ok 4 : (int, string) Belt.Result.t) in
        let someFloat = Some 5.0 in
        describe "Hush" (fun () ->
            it "should convert Error result to None" (fun () ->
                expect (Result.hush errResult) |> to_be None);
            it "should convert Success result to Some" (fun () ->
                expect (Result.hush okResult) |> to_be (Some 4)));
        describe "Note" (fun () ->
            it "should convert None to Error result" (fun () ->
                expect (Result.note "ERROR" None) |> to_be (Belt.Result.Error "ERROR"));
            it "should convert Some to Ok result" (fun () ->
                expect (Result.note "ERROR" someFloat) |> to_be (Belt.Result.Ok 5.0)))))
