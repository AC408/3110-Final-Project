open OUnit2
open Chess.Piece
open Chess.Command
open Chess.Board

(**These are formatted as piece options*)
let wqtestopt =
  Some (place_piece (Some ('d', 1)) White Queen "|  ♕   " true)

let wktestopt =
  Some (place_piece (Some ('e', 1)) White King "|  ♔   " true)

let wbtestopt =
  Some (place_piece (Some ('c', 1)) White Bishop "|  ♗   " true)

let wkntestopt =
  Some (place_piece (Some ('b', 1)) White Knight "|  ♘   " true)

let wrtestopt =
  Some (place_piece (Some ('a', 1)) White Rook "|  ♖   " true)

let wptestopt =
  Some (place_piece (Some ('a', 2)) White Pawn "|  ♙   " true)

let bqtestopt =
  Some (place_piece (Some ('d', 8)) Black Queen "|  ♛   " true)

let bktestopt =
  Some (place_piece (Some ('e', 8)) Black King "|  ♚   " true)

let bbtestopt =
  Some (place_piece (Some ('c', 8)) Black Bishop "|  ♝   " true)

let bkntestopt =
  Some (place_piece (Some ('b', 8)) Black Knight "|  ♞   " true)

let brtestopt =
  Some (place_piece (Some ('a', 8)) Black Rook "|  ♜   " true)

let bptestopt =
  Some (place_piece (Some ('a', 7)) Black Pawn "|  ♟   " true)

(**These are not written as options -- just pieces*)

let wqtest = place_piece (Some ('d', 1)) White Queen "|  ♕   " true
let wktest = place_piece (Some ('e', 1)) White King "|  ♔   " true
let wbtest = place_piece (Some ('c', 1)) White Bishop "|  ♗   " true
let wkntest = place_piece (Some ('b', 1)) White Knight "|  ♘   " true
let wrtest = place_piece (Some ('a', 1)) White Rook "|  ♖   " true
let wptest = place_piece (Some ('a', 2)) White Pawn "|  ♙   " true
let bqtest = place_piece (Some ('d', 8)) Black Queen "|  ♛   " false
let bktest = place_piece (Some ('e', 8)) Black King "|  ♚   " false
let bbtest = place_piece (Some ('c', 8)) Black Bishop "|  ♝   " false
let bkntest = place_piece (Some ('b', 8)) Black Knight "|  ♞   " false
let brtest = place_piece (Some ('a', 8)) Black Rook "|  ♜   " false
let bptest = place_piece (Some ('a', 7)) Black Pawn "|  ♟   " false

(**[rep_tests name pieceopt output_rep] constructs an OUnit test named
   [name] that asserts the quality of [output_rep] with [rep pieceopt].*)
let rep_tests
    (name : string)
    (pieceopt : piece option)
    (output_rep : string) : test =
  name >:: fun _ -> assert_equal output_rep (rep pieceopt)

let rep_tests =
  [
    rep_tests "WQ" wqtestopt "|  ♕   ";
    rep_tests "WK" wktestopt "|  ♔   ";
    rep_tests "WB" wbtestopt "|  ♗   ";
    rep_tests "WKn" wkntestopt "|  ♘   ";
    rep_tests "WR" wrtestopt "|  ♖   ";
    rep_tests "WP" wptestopt "|  ♙   ";
    rep_tests "BQ" bqtestopt "|  ♛   ";
    rep_tests "BK" bktestopt "|  ♚   ";
    rep_tests "BB" bbtestopt "|  ♝   ";
    rep_tests "BKn" bkntestopt "|  ♞   ";
    rep_tests "BR" brtestopt "|  ♜   ";
    rep_tests "BP" bptestopt "|  ♟   ";
  ]

(**[get_level_tests name piece output_lev] constructs an OUnit test
   named [name] that asserts the quality of [output_lev] with
   [get_level piece].*)
let get_level_tests (name : string) (piece : piece) (output_lev : level)
    : test =
  name >:: fun _ -> assert_equal output_lev (get_level piece)

let get_level_tests =
  [
    get_level_tests "WQ" wqtest Queen;
    get_level_tests "WK" wktest King;
    get_level_tests "WB" wbtest Bishop;
    get_level_tests "WKn" wkntest Knight;
    get_level_tests "WR" wrtest Rook;
    get_level_tests "WP" wptest Pawn;
    get_level_tests "BQ" bqtest Queen;
    get_level_tests "BK" bktest King;
    get_level_tests "BB" bbtest Bishop;
    get_level_tests "BKn" bkntest Knight;
    get_level_tests "BR" brtest Rook;
    get_level_tests "BP" bptest Pawn;
  ]

(**[get_color_tests name piece output_col] constructs an OUnit test
   named [name] that asserts the quality of [output_col] with
   [get_color piece].*)
let get_color_tests (name : string) (piece : piece) (output_col : color)
    : test =
  name >:: fun _ -> assert_equal output_col (get_color piece)

let get_color_tests =
  [
    get_color_tests "WQ" wqtest White;
    get_color_tests "WK" wktest White;
    get_color_tests "WB" wbtest White;
    get_color_tests "WKn" wkntest White;
    get_color_tests "WR" wrtest White;
    get_color_tests "WP" wptest White;
    get_color_tests "BQ" bqtest Black;
    get_color_tests "BK" bktest Black;
    get_color_tests "BB" bbtest Black;
    get_color_tests "BKn" bkntest Black;
    get_color_tests "BR" brtest Black;
    get_color_tests "BP" bptest Black;
  ]

(**[get_pos_tests name piece output_pos] constructs an OUnit test named
   [name] that asserts the quality of [output_pos] with
   [get_position piece].*)
let get_pos_tests
    (name : string)
    (piece : piece)
    (output_pos : position) : test =
  name >:: fun _ -> assert_equal output_pos (get_position piece)

let get_pos_tests =
  [
    get_pos_tests "WQ" wqtest (Some ('d', 1));
    get_pos_tests "WK" wktest (Some ('e', 1));
    get_pos_tests "WB" wbtest (Some ('c', 1));
    get_pos_tests "WKn" wkntest (Some ('b', 1));
    get_pos_tests "WR" wrtest (Some ('a', 1));
    get_pos_tests "WP" wptest (Some ('a', 2));
    get_pos_tests "BQ" bqtest (Some ('d', 8));
    get_pos_tests "BK" bktest (Some ('e', 8));
    get_pos_tests "BB" bbtest (Some ('c', 8));
    get_pos_tests "BKn" bkntest (Some ('b', 8));
    get_pos_tests "BR" brtest (Some ('a', 8));
    get_pos_tests "BP" bptest (Some ('a', 7));
  ]

(**[place_piece_tests name position color level rep move output_pc]
   constructs an OUnit test named [name] that asserts the quality of
   [output_pc] with [place_piece position color level rep move].*)
let place_piece_tests
    (name : string)
    (position : position)
    (color : color)
    (level : level)
    (rep : string)
    (move : bool)
    (output_pc : piece) : test =
  name >:: fun _ ->
  assert_equal output_pc (place_piece position color level rep move)

let place_piece_tests =
  [
    place_piece_tests "WQ"
      (Some ('d', 1))
      White Queen "|  ♕   " true
      {
        position = Some ('d', 1);
        color = White;
        level = Queen;
        rep = "|  ♕   ";
        moved = true;
      };
    place_piece_tests "WK"
      (Some ('e', 1))
      White King "|  ♔   " true
      {
        position = Some ('e', 1);
        color = White;
        level = King;
        rep = "|  ♔   ";
        moved = true;
      };
    place_piece_tests "WB"
      (Some ('c', 1))
      White Bishop "|  ♗   " true
      {
        position = Some ('c', 1);
        color = White;
        level = Bishop;
        rep = "|  ♗   ";
        moved = true;
      };
    place_piece_tests "WKn"
      (Some ('b', 1))
      White Knight "|  ♘   " true
      {
        position = Some ('b', 1);
        color = White;
        level = Knight;
        rep = "|  ♘   ";
        moved = true;
      };
    place_piece_tests "WR"
      (Some ('a', 1))
      White Rook "|  ♖   " true
      {
        position = Some ('a', 1);
        color = White;
        level = Rook;
        rep = "|  ♖   ";
        moved = true;
      };
    place_piece_tests "WP"
      (Some ('a', 2))
      White Pawn "|  ♙   " true
      {
        position = Some ('a', 2);
        color = White;
        level = Pawn;
        rep = "|  ♙   ";
        moved = true;
      };
    place_piece_tests "BQ"
      (Some ('d', 8))
      Black Queen "|  ♛   " false
      {
        position = Some ('d', 8);
        color = Black;
        level = Queen;
        rep = "|  ♛   ";
        moved = false;
      };
    place_piece_tests "BK"
      (Some ('e', 8))
      Black King "|  ♚   " false
      {
        position = Some ('e', 8);
        color = Black;
        level = King;
        rep = "|  ♚   ";
        moved = false;
      };
    place_piece_tests "BB"
      (Some ('c', 8))
      Black Bishop "|  ♝   " false
      {
        position = Some ('c', 8);
        color = Black;
        level = Bishop;
        rep = "|  ♝   ";
        moved = false;
      };
    place_piece_tests "BKn"
      (Some ('b', 8))
      Black Knight "|  ♞   " false
      {
        position = Some ('b', 8);
        color = Black;
        level = Knight;
        rep = "|  ♞   ";
        moved = false;
      };
    place_piece_tests "BR"
      (Some ('a', 8))
      Black Rook "|  ♜   " false
      {
        position = Some ('a', 8);
        color = Black;
        level = Rook;
        rep = "|  ♜   ";
        moved = false;
      };
    place_piece_tests "BP"
      (Some ('a', 7))
      Black Pawn "|  ♟   " false
      {
        position = Some ('a', 7);
        color = Black;
        level = Pawn;
        rep = "|  ♟   ";
        moved = false;
      };
  ]

(**[moved_tests name piece output_mov] constructs an OUnit test named
   [name] that asserts the quality of [output_mov] with
   [has_moved piece].*)
let moved_tests (name : string) (piece : piece) (output_mov : bool) :
    test =
  name >:: fun _ -> assert_equal output_mov (have_moved piece)

let moved_tests =
  [
    moved_tests "WQ" wqtest true;
    moved_tests "WK" wktest true;
    moved_tests "WB" wbtest true;
    moved_tests "WKn" wkntest true;
    moved_tests "WR" wrtest true;
    moved_tests "WP" wptest true;
    moved_tests "BQ" bqtest false;
    moved_tests "BK" bktest false;
    moved_tests "BB" bbtest false;
    moved_tests "BKn" bkntest false;
    moved_tests "BR" brtest false;
    moved_tests "BP" bptest false;
  ]

(**[remove_blank_tests name strlist output_strlist] constructs an OUnit
   test named [name] that asserts the quality of [output_strlist] with
   [remove_blank strlist].*)
let remove_blank_tests
    (name : string)
    (strlist : string list)
    (output_strlist : string list) : test =
  name >:: fun _ -> assert_equal output_strlist (remove_blank strlist)

let remove_blank_tests =
  [
    remove_blank_tests "empty" [] [];
    remove_blank_tests "all empty strings" [ ""; ""; "" ] [];
    remove_blank_tests "one empty string" [ "" ] [];
    remove_blank_tests "one element" [ "hi" ] [ "hi" ];
    remove_blank_tests "multiple elements" [ "hi"; "everyone" ]
      [ "hi"; "everyone" ];
    remove_blank_tests "first string is empty" [ ""; "hi" ] [ "hi" ];
    remove_blank_tests "last string is empty" [ "hi"; "" ] [ "hi" ];
    remove_blank_tests "middle string is empty" [ "hi"; ""; "all" ]
      [ "hi"; "all" ];
    remove_blank_tests "alternating empty and filled"
      [ "hi"; ""; "all"; ""; "!" ]
      [ "hi"; "all"; "!" ];
  ]

(**[explode_tests name str output_charlist] constructs an OUnit test
   named [name] that asserts the quality of [output_charlist] with
   [explode strlist].*)
let explode_tests
    (name : string)
    (str : string)
    (output_charlist : char list) : test =
  name >:: fun _ -> assert_equal output_charlist (explode str)

let explode_tests =
  [
    explode_tests "empty" "" [];
    explode_tests "one space" " " [ ' ' ];
    explode_tests "multiple spaces" "   " [ ' '; ' '; ' ' ];
    explode_tests "one char" "h" [ 'h' ];
    explode_tests "multiple char, same" "hhh" [ 'h'; 'h'; 'h' ];
    explode_tests "multiple char, different" "welcomers"
      [ 'w'; 'e'; 'l'; 'c'; 'o'; 'm'; 'e'; 'r'; 's' ];
    explode_tests "intermingled - start with space" " hello"
      [ ' '; 'h'; 'e'; 'l'; 'l'; 'o' ];
    explode_tests "intermingled - end with space" "hello "
      [ 'h'; 'e'; 'l'; 'l'; 'o'; ' ' ];
    explode_tests "alternating space and filled" "h e l l o"
      [ 'h'; ' '; 'e'; ' '; 'l'; ' '; 'l'; ' '; 'o' ];
  ]

let tests =
  "test suite for full project"
  >::: List.flatten
         [
           rep_tests;
           get_level_tests;
           get_color_tests;
           get_pos_tests;
           place_piece_tests;
           moved_tests;
           remove_blank_tests;
           explode_tests;
         ]

let _ = run_test_tt_main tests
