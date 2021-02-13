open BsMocha.Mocha
open BsChai.Expect.Expect
open BsChai.Expect.Combos.End
open! Functors

;;
describe "Functions" (fun () ->
    describe "Traversable" (fun () ->
        describe "Array" (fun () ->
            describe "Scan" (fun () ->
                let scan_left, scan_right =
                  let open ArrayF.Int.Functions.Scan in
                  scan_left, scan_right
                in
                describe "scan_left" (fun () ->
                    it "should scan from the left" (fun () ->
                        expect (scan_left ( + ) 0 [|1; 2; 3|]) |> to_be [|1; 3; 6|];
                        expect (scan_left ( - ) 10 [|1; 2; 3|]) |> to_be [|9; 7; 4|]));
                describe "scan_right" (fun () ->
                    it "should scan from the right (array)" (fun () ->
                        expect (scan_right ( + ) 0 [|1; 2; 3|]) |> to_be [|6; 5; 3|];
                        expect (scan_right (Function.flip ( - )) 10 [|1; 2; 3|])
                        |> to_be [|4; 5; 7|]))));
        describe "List" (fun () ->
            describe "Scan" (fun () ->
                let scan_left, scan_right =
                  let open ListF.Int.Functions.Scan in
                  scan_left, scan_right
                in
                describe "scan_left" (fun () ->
                    it "should scan from the left" (fun () ->
                        expect (scan_left ( + ) 0 [1; 2; 3]) |> to_be [1; 3; 6];
                        expect (scan_left ( - ) 10 [1; 2; 3]) |> to_be [9; 7; 4]));
                describe "scan_right" (fun () ->
                    it "should scan from the right (array)" (fun () ->
                        expect (scan_right ( + ) 0 [1; 2; 3]) |> to_be [6; 5; 3];
                        expect (scan_right (Function.flip ( - )) 10 [1; 2; 3]) |> to_be [4; 5; 7]))))))
