open OUnit2
open Piece
open Command
open Board

let rep_tests = "test suite for rep" >::: [
  "None" >:: (fun _ -> assert_equal NoPiece (rep None));
]

let _ = run_test_tt_main rep_tests