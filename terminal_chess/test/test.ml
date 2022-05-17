open OUnit2
open Chess.Piece
open Chess.Command
open Chess.Board
open Chess.Display

(**These are formatted as piece options*)
let wqtestopt =
  Some
    (place_piece "whitequeen" { row = 1; col = "d" } "White" "Queen"
       "|  ♕   " true)

let wktestopt =
  Some
    (place_piece "whiteking" { row = 1; col = "e" } "White" "King"
       "|  ♔   " true)

let wbtestopt =
  Some
    (place_piece "whitebishop1" { row = 1; col = "c" } "White" "Bishop"
       "|  ♗   " true)

let wkntestopt =
  Some
    (place_piece "whiteknight1" { row = 1; col = "b" } "White" "Knight"
       "|  ♘   " true)

let wrtestopt =
  Some
    (place_piece "whiterook1" { row = 1; col = "a" } "White" "Rook"
       "|  ♖   " true)

let wptestopt =
  Some
    (place_piece "whitepawn1" { row = 2; col = "a" } "White" "Pawn"
       "|  ♙   " true)

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
let model1 = { moves = 0; turn = White }
let model1b = { moves = 1; turn = Black }
let model2 = { moves = 2; turn = White }
let model2b = { moves = 3; turn = Black }
let model3 = { moves = 0; turn = Black }
let model3b = { moves = 1; turn = White }
let model4 = { moves = 5; turn = Black }
let model4b = { moves = 6; turn = White }
let model5 = { moves = 1; turn = White }
let model5b = { moves = 2; turn = Black }
let model6 = { moves = 100; turn = Black }
let model6b = { moves = 101; turn = White }
let model7 = { moves = 1000; turn = White }
let model7b = { moves = 1001; turn = Black }
let model8 = { moves = 15; turn = Black }
let model8b = { moves = 16; turn = White }
let model9 = { moves = 17; turn = White }
let model9b = { moves = 18; turn = Black }
let model10 = { moves = 30; turn = Black }
let model10b = { moves = 31; turn = White }
let cl1 = [ '('; 'a'; ','; '1'; ')' ]
let cl2 = [ '('; 'b'; ','; '5'; ')' ]
let cl3 = [ '('; '6'; ','; '*'; ')' ]
let cl4 = [ '('; 'g'; ','; '9'; ')' ]
let cl5 = [ '('; '5'; ','; ')'; ')' ]
let cl6 = [ '('; '('; ','; ')'; ')' ]
let cl7 = [ '('; 'j'; ','; '('; ')' ]
let cl8 = [ '('; '4'; ','; '5'; ')' ]
let cl9 = [ '('; 'm'; ','; 'm'; ')' ]
let cl10 = [ '('; '*'; ','; '1'; ')' ]

let wkgrid =
  [|
    [| None; None; None; None; wktestopt; None; None; None |];
    make_empty_row ();
    make_empty_row ();
    make_empty_row ();
    make_empty_row ();
    make_empty_row ();
    make_empty_row ();
    make_empty_row ();
  |]

let wtestgrid =
  let new_arr = Array.init 8 (fun x -> Array.get wkgrid x) in
  Array.set new_arr 0
    [|
      wrtestopt;
      wkntestopt;
      wbtestopt;
      wqtestopt;
      wktestopt;
      wbtestopt;
      wkntestopt;
      wrtestopt;
    |];
  Array.set new_arr 1 (Array.make 8 wptestopt);
  new_arr

let btestgrid =
  let new_arr = Array.init 8 (fun x -> Array.get wtestgrid x) in
  Array.set new_arr 7
    [|
      brtestopt;
      bkntestopt;
      bbtestopt;
      bqtestopt;
      bktestopt;
      bbtestopt;
      bkntestopt;
      brtestopt;
    |];
  Array.set new_arr 1 (Array.make 8 bptestopt);
  new_arr

let testgrid = Array.append wtestgrid btestgrid

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

(**[get_turn_tests name board output_turn] constructs an OUnit test
   named [name] that asserts the quality of [output_turn] with
   [get_turn board].*)
let get_turn_tests
    (name : string)
    (board : model)
    (output_turn : string) : test =
  name >:: fun _ -> assert_equal output_turn (get_turn board)

let get_turn_tests =
  [
    get_turn_tests "mod1" model1 "White";
    get_turn_tests "mod1b" model1b "Black";
    get_turn_tests "mod2" model2 "White";
    get_turn_tests "mod2b" model2b "Black";
    get_turn_tests "mod3" model3 "Black";
    get_turn_tests "mod3b" model3b "White";
    get_turn_tests "mod4" model4 "Black";
    get_turn_tests "mod4b" model4b "White";
    get_turn_tests "mod5" model5 "White";
    get_turn_tests "mod5b" model5b "Black";
    get_turn_tests "mod6" model6 "Black";
    get_turn_tests "mod6b" model6b "White";
    get_turn_tests "mod7" model7 "White";
    get_turn_tests "mod7b" model7b "Black";
    get_turn_tests "mod8" model8 "Black";
    get_turn_tests "mod8b" model8b "White";
    get_turn_tests "mod9" model9 "White";
    get_turn_tests "mod9b" model9b "Black";
    get_turn_tests "mod10" model10 "Black";
    get_turn_tests "mod10b" model10b "White";
  ]

(**[update_tests name board change output_board] constructs an OUnit
   test named [name] that asserts the quality of [output_board] with
   [update_turn board change].*)
let update_tests
    (name : string)
    (board : model)
    (change : change)
    (output_board : model) : test =
  name >:: fun _ -> assert_equal output_board (update_turn board change)

let update_tests =
  [
    update_tests "mod1" model1 Change model1b;
    update_tests "mod2" model2 Change model2b;
    update_tests "mod3" model3 Change model3b;
    update_tests "mod4" model4 Change model4b;
    update_tests "mod5" model5 Change model5b;
    update_tests "mod6" model6 Change model6b;
    update_tests "mod7" model7 Change model7b;
    update_tests "mod8" model8 Change model8b;
    update_tests "mod9" model9 Change model9b;
    update_tests "mod10" model10 Change model10b;
  ]

(**[cf_tests name charlist output_str] constructs an OUnit test named
   [name] that asserts the quality of [output_str] with
   [check_format charlist].*)
let cf_tests
    (name : string)
    (charlist : char list)
    (output_str : string) : test =
  name >:: fun _ -> assert_equal output_str (check_format charlist)

let cf_tests =
  [
    cf_tests "a1" cl1 "a1";
    cf_tests "b5" cl2 "b5";
    cf_tests "6*" cl3 "6*";
    cf_tests "g9" cl4 "g9";
    cf_tests "5)" cl5 "5)";
    cf_tests "()" cl6 "()";
    cf_tests "j(" cl7 "j(";
    cf_tests "45" cl8 "45";
    cf_tests "mm" cl9 "mm";
    cf_tests "*1" cl10 "*1";
  ]

(**[cvm_tests name str output_str] constructs an OUnit test named [name]
   that asserts the quality of [output_str] with [check_valid_move str].*)
let cvm_tests (name : string) (str : string list) (output_str : string)
    : test =
  name >:: fun _ -> assert_equal output_str (check_valid_move str)

let cvm_tests =
  [
    cvm_tests "a1b2" [ "move"; "(a,1)"; "(b,2)" ] "a1b2";
    cvm_tests "b1g1" [ "move"; "(b,1)"; "(g,1)" ] "b1g1";
    cvm_tests "c6f6" [ "move"; "(c,6)"; "(f,6)" ] "c6f6";
    cvm_tests "g2g9" [ "move"; "(g,2)"; "(g,9)" ] "g2g9";
    cvm_tests "d4e8" [ "move"; "(d,4)"; "(e,8)" ] "d4e8";
    cvm_tests "f4g5" [ "move"; "(f,4)"; "(g,5)" ] "f4g5";
    cvm_tests "e3e3" [ "move"; "(e,3)"; "(e,3)" ] "e3e3";
    cvm_tests "h8a1" [ "move"; "(h,8)"; "(a,1)" ] "h8a1";
    cvm_tests "h8f6" [ "move"; "(h,8)"; "(f,6)" ] "h8f6";
    cvm_tests "mmmm" [ "move"; "(m,m)"; "(m,m)" ] "mmmm";
  ]

(**[cq_tests name str output_bool] constructs an OUnit test named [name]
   that asserts the quality of [output_bool] with [check_quit str].*)
let cq_tests (name : string) (str : string) (output_bool : bool) : test
    =
  name >:: fun _ -> assert_equal output_bool (check_quit str)

let cq_tests =
  [
    cq_tests "T" "quit" true;
    cq_tests "F" "" false;
    cq_tests "F" "Quit" false;
    cq_tests "F" "QUIT" false;
    cq_tests "F" "qut" false;
    cq_tests "F" "qit" false;
    cq_tests "F" "exit" false;
    cq_tests "F" "QuIt" false;
    cq_tests "F" "q uit" false;
    cq_tests "F" " quit" false;
  ]

(**[parse_mod_tests name str output_str] constructs an OUnit test named
   [name] that asserts the quality of [output_str] with [parse_mod str].*)
let parse_mod_tests (name : string) (str : string) (output_str : string)
    : test =
  name >:: fun _ -> assert_equal output_str (parse_mod str)

let parse_mod_tests =
  [
    parse_mod_tests "a1b2" "move (a,1) (b,2)" "a1b2";
    parse_mod_tests "b1g1" "move (b,1) (g,1)" "b1g1";
    parse_mod_tests "c6f6" "move (c,6) (f,6)" "c6f6";
    parse_mod_tests "g2g9" "move (g,2) (g,9)" "g2g9";
    parse_mod_tests "d4e8" "move (d,4) (e,8)" "d4e8";
    parse_mod_tests "f4g5" "move (f,4) (g,5)" "f4g5";
    parse_mod_tests "e3e3" "move (e,3) (e,3)" "e3e3";
    parse_mod_tests "h8a1" "move (h,8) (a,1)" "h8a1";
    parse_mod_tests "h8f6" "move (h,8) (f,6)" "h8f6";
    parse_mod_tests "mmmm" "move (m,m) (m,m)" "mmmm";
  ]

(**[parse_tests name str output_strlist] constructs an OUnit test named
   [name] that asserts the quality of [output_strlist] with [parse str].*)
let parse_tests
    (name : string)
    (str : string)
    (output_strlist : string list) : test =
  name >:: fun _ -> assert_equal output_strlist (parse str)

let parse_tests =
  [
    parse_tests "empty" "" [];
    parse_tests "one word no space" "hello" [ "hello" ];
    parse_tests "one word space" "hello " [ "hello" ];
    parse_tests "two words" "hello everyone" [ "hello"; "everyone" ];
    parse_tests "three words" "wow hello all" [ "wow"; "hello"; "all" ];
    parse_tests "smushed together" "it'sreallysonicetomeetyou"
      [ "it'sreallysonicetomeetyou" ];
    parse_tests "missing spaces" "it'sreallyso nicetomeet you"
      [ "it'sreallyso"; "nicetomeet"; "you" ];
    parse_tests "wrong spaces" "h ello it'smeLu igi"
      [ "h"; "ello"; "it'smeLu"; "igi" ];
    parse_tests "alternating spaces" "h e l l o"
      [ "h"; "e"; "l"; "l"; "o" ];
    parse_tests "single space" " " [];
    parse_tests "multiple spaces" "   " [];
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
           get_turn_tests;
           update_tests;
           cf_tests;
           cvm_tests;
           cq_tests;
           parse_tests;
           parse_mod_tests;
         ]

let _ = run_test_tt_main tests
