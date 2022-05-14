open OUnit2
open Chess.Piece
open Chess.Command
open Chess.Board

let wqtest =
  Some (place_piece (Some ('d', 1)) White Queen "|  ♕   " true)

let wktest =
  Some (place_piece (Some ('e', 1)) White King "|  ♔   " true)

let wbtest =
  Some (place_piece (Some ('c', 1)) White Bishop "|  ♗   " true)

let wkntest =
  Some (place_piece (Some ('b', 1)) White Knight "|  ♘   " true)

let wrtest =
  Some (place_piece (Some ('a', 1)) White Rook "|  ♖   " true)

let wptest =
  Some (place_piece (Some ('a', 2)) White Pawn "|  ♙   " true)

let bqtest =
  Some (place_piece (Some ('d', 1)) Black Queen "|  ♛   " true)

let bktest =
  Some (place_piece (Some ('e', 1)) Black King "|  ♚   " true)

let bbtest =
  Some (place_piece (Some ('c', 1)) Black Bishop "|  ♝   " true)

let bkntest =
  Some (place_piece (Some ('b', 1)) Black Knight "|  ♞   " true)

let brtest =
  Some (place_piece (Some ('a', 1)) Black Rook "|  ♜   " true)

let bptest =
  Some (place_piece (Some ('a', 2)) Black Pawn "|  ♟   " true)

let rep_tests =
  "test suite for rep"
  >::: [
         ("WQ" >:: fun _ -> assert_equal "|  ♕   " (rep wqtest));
         ("WK" >:: fun _ -> assert_equal "|  ♔   " (rep wktest));
         ("WB" >:: fun _ -> assert_equal "|  ♗   " (rep wbtest));
         ("WKn" >:: fun _ -> assert_equal "|  ♘   " (rep wkntest));
         ("WR" >:: fun _ -> assert_equal "|  ♖   " (rep wrtest));
         ("WP" >:: fun _ -> assert_equal "|  ♙   " (rep wptest));
         ("BQ" >:: fun _ -> assert_equal "|  ♛   " (rep bqtest));
         ("BK" >:: fun _ -> assert_equal "|  ♚   " (rep bktest));
         ("BB" >:: fun _ -> assert_equal "|  ♝   " (rep bbtest));
         ("BKn" >:: fun _ -> assert_equal "|  ♞   " (rep bkntest));
         ("BR" >:: fun _ -> assert_equal "|  ♜   " (rep brtest));
         ("BP" >:: fun _ -> assert_equal "|  ♟   " (rep bptest));
       ]

let _ = run_test_tt_main rep_tests
