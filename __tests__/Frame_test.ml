open Jest
open Expect
open Frame

let _ =
  describe "Frame" (fun () ->
      test "should not raise an exception on out-of-bounds line numbers"
        (fun () ->
          let frame =
            { filename= Some "foo.js"
            ; line= Some 4
            ; col= Some 10
            ; context= [(2, "abc"); (3, "def"); (4, "ghi")] }
          in
          let fileContents = [|"abc"; "def"; "ghi"|] in
          expect (fun () -> fix fileContents frame) |> not_ |> toThrow ) ;
      test "should fix line numbers" (fun () ->
          let frame =
            { filename= Some "foo.js"
            ; line= Some 3
            ; col= Some 10
            ; context= [(2, "abc"); (3, "def"); (4, "ghi")] }
          in
          let fileContents = [|"abc"; "def"; "ghi"; "jkl"|] in
          expect (fix fileContents frame |> getLineNumber) |> toBe (Some 2) )
  )
