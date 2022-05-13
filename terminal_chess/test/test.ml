open OUnit2
open Piece
open Command
open Board


(** [index_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [index input]. *)
let index_test (name : string) (input : char) (expected_output : int) :
    test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (index input) ~printer:string_of_int
  
  let suite = "terminal test chess suite" >::: tests
let _ = run_test_tt_main suite
