let in_game = false
(*toggle between parsing from main menu and command*)

open Chess
open Piece
open Board
open Display
open Command

let rec print_list = function
  | [] -> ()
  | e :: l ->
      print_string e;
      print_string " ";
      print_list l

let color_matcher board space =
  match space with
  | None ->
      print_endline "calling None";
      raise NoPiece
  | Some space -> board.model.turn <> space.color

let move_into_check piece cmd king r1 r2 r3 r4 r5 r6 r7 r8 =
  let p =
    match piece with
    | None -> failwith "input not a piece"
    | Some pc -> pc
  in
  let k = ref king in
  let a_bp = ref [] in
  let a_wp = ref [] in
  let arr1 = Array.copy r1 in
  let arr2 = Array.copy r2 in
  let arr3 = Array.copy r3 in
  let arr4 = Array.copy r4 in
  let arr5 = Array.copy r5 in
  let arr6 = Array.copy r6 in
  let arr7 = Array.copy r7 in
  let arr8 = Array.copy r8 in
  let o_r =
    Command.checkn3 cmd arr1 arr2 arr3 arr4 arr5 arr6 arr7 arr8
  in
  let moved_piece =
    Piece.place_piece
      (Some (cmd.[3], int_of_char cmd.[2] - int_of_char '0'))
      (Piece.get_color p) (Piece.get_level p) (Piece.get_rep p) true
  in
  if p = king then k := moved_piece;
  Some moved_piece |> Array.set o_r (Char.code cmd.[3] - Char.code 'a');
  None
  |> Array.set
       (Command.checkn1 cmd arr1 arr2 arr3 arr4 arr5 arr6 arr7 arr8)
       (Char.code cmd.[1] - Char.code 'a');
  Array.iter
    (fun y ->
      match y with
      | None -> ()
      | Some x ->
          if x.color = White then a_wp := x :: !a_wp
          else a_bp := x :: !a_bp)
    arr1;
  Array.iter
    (fun y ->
      match y with
      | None -> ()
      | Some x ->
          if x.color = White then a_wp := x :: !a_wp
          else a_bp := x :: !a_bp)
    arr2;
  Array.iter
    (fun y ->
      match y with
      | None -> ()
      | Some x ->
          if x.color = White then a_wp := x :: !a_wp
          else a_bp := x :: !a_bp)
    arr3;
  Array.iter
    (fun y ->
      match y with
      | None -> ()
      | Some x ->
          if x.color = White then a_wp := x :: !a_wp
          else a_bp := x :: !a_bp)
    arr4;
  Array.iter
    (fun y ->
      match y with
      | None -> ()
      | Some x ->
          if x.color = White then a_wp := x :: !a_wp
          else a_bp := x :: !a_bp)
    arr5;
  Array.iter
    (fun y ->
      match y with
      | None -> ()
      | Some x ->
          if x.color = White then a_wp := x :: !a_wp
          else a_bp := x :: !a_bp)
    arr6;
  Array.iter
    (fun y ->
      match y with
      | None -> ()
      | Some x ->
          if x.color = White then a_wp := x :: !a_wp
          else a_bp := x :: !a_bp)
    arr7;
  Array.iter
    (fun y ->
      match y with
      | None -> ()
      | Some x ->
          if x.color = White then a_wp := x :: !a_wp
          else a_bp := x :: !a_bp)
    arr8;
  match Piece.get_color king with
  | White ->
      incheck !a_bp !k true arr1 arr2 arr3 arr4 arr5 arr6 arr7 arr8
  | Black ->
      incheck !a_wp !k true arr1 arr2 arr3 arr4 arr5 arr6 arr7 arr8

let rec promote_check input piece1 =
  match piece1 with
  | None ->
      print_endline
        "Sorry, you can't promote an empty space. Try again!";
      promote_check input piece1
  | Some piece -> (
      let prom_command = read_line () in
      match prom_command with
      | "Queen" ->
          if Piece.get_color piece = White then
            Some
              (Piece.place_piece
                 (Piece.get_position piece)
                 (Piece.get_color piece) Queen "|  ♕   " true)
          else
            Some
              (Piece.place_piece
                 (Piece.get_position piece)
                 (Piece.get_color piece) Queen "|  ♛   " true)
      | "Bishop" ->
          if Piece.get_color piece = White then
            Some
              (Piece.place_piece
                 (Piece.get_position piece)
                 (Piece.get_color piece) Bishop "|  ♗   " true)
          else
            Some
              (Piece.place_piece
                 (Piece.get_position piece)
                 (Piece.get_color piece) Bishop "|  ♝   " true)
      | "Knight" ->
          if Piece.get_color piece = White then
            Some
              (Piece.place_piece
                 (Piece.get_position piece)
                 (Piece.get_color piece) Knight "|  ♘   " true)
          else
            Some
              (Piece.place_piece
                 (Piece.get_position piece)
                 (Piece.get_color piece) Knight "|  ♞   " true)
      | "Rook" ->
          if Piece.get_color piece = White then
            Some
              (Piece.place_piece
                 (Piece.get_position piece)
                 (Piece.get_color piece) Rook "|  ♖   " true)
          else
            Some
              (Piece.place_piece
                 (Piece.get_position piece)
                 (Piece.get_color piece) Rook "|  ♜   " true)
      | _ ->
          print_endline
            "Sorry, that's not a valid rank to promote your pawn to. \
             Try again!";
          promote_check input piece1)

let rec filled_input board space =
  try color_matcher board space with
  | NoPiece ->
      print_endline
        "Sorry, there's no piece there. Check your input and try again.";
      mover_init board

and wrong_input str =
  try Command.parse_mod str with
  | Command.InvalidInput ->
      print_endline
        "Sorry, you haven't entered a valid move. Check your \
         formatting!";
      "wrong move"

and char_in_range str =
  Char.code str.[1] > Char.code 'h'
  || Char.code str.[1] < Char.code 'a'
  || Char.code str.[3] > Char.code 'h'
  || Char.code str.[3] < Char.code 'a'

and mover_init board =
  let avail_wp = ref [] in
  let avail_bp = ref [] in
  let w_k = ref whiteking in
  let b_k = ref blackking in
  print_newline ();
  let whoseturn =
    "Currently it is " ^ Board.get_turn board.model ^ "'s turn."
  in
  print_endline whoseturn;
  print_endline "Please enter a move. Example format: move (3,b) (4,c) ";
  let in_command = read_line () in
  if Command.check_quit in_command then Stdlib.exit 0
  else if wrong_input in_command = "wrong move" then mover_init board
  else if
    Command.parse in_command |> List.hd <> "move"
    || Command.parse_mod in_command |> char_in_range
  then (
    print_endline
      "Sorry, that doesn't work. Check your input and try again.";
    mover_init board)
  else if wrong_input in_command = "wrong move" then mover_init board
  else
    let input = Command.parse_mod in_command in
    let oc_rel_a = Char.code input.[3] - Char.code 'a' in
    let ic_rel_a = Char.code input.[1] - Char.code 'a' in
    if
      ic_rel_a |> Array.get (Command.check1 input) |> filled_input board
    then (
      print_endline "Sorry, it's not your turn. Try again later!.";
      mover_init board)
    else
      let input = Command.parse_mod in_command in
      let i_pr = Command.check1 input in
      (*input piecerow*)
      let o_pr = Command.check3 input in
      (*output piecerow*)
      let i_p = Array.get i_pr ic_rel_a in
      (*ip*)
      let o_p = Array.get o_pr oc_rel_a in
      (*op*)
      let clr = board.model.turn in
      Array.iter
        (fun y ->
          match y with
          | None -> ()
          | Some x ->
              if x.color = White then (
                if x.level = King then w_k := x;
                avail_wp := x :: !avail_wp)
              else (
                if x.level = King then b_k := x;
                avail_bp := x :: !avail_bp))
        board.r1;
      Array.iter
        (fun y ->
          match y with
          | None -> ()
          | Some x ->
              if x.color = White then (
                if x.level = King then w_k := x;
                avail_wp := x :: !avail_wp)
              else (
                if x.level = King then b_k := x;
                avail_bp := x :: !avail_bp))
        board.r2;
      Array.iter
        (fun y ->
          match y with
          | None -> ()
          | Some x ->
              if x.color = White then (
                if x.level = King then w_k := x;
                avail_wp := x :: !avail_wp)
              else (
                if x.level = King then b_k := x;
                avail_bp := x :: !avail_bp))
        board.r3;
      Array.iter
        (fun y ->
          match y with
          | None -> ()
          | Some x ->
              if x.color = White then (
                if x.level = King then w_k := x;
                avail_wp := x :: !avail_wp)
              else (
                if x.level = King then b_k := x;
                avail_bp := x :: !avail_bp))
        board.r4;
      Array.iter
        (fun y ->
          match y with
          | None -> ()
          | Some x ->
              if x.color = White then (
                if x.level = King then w_k := x;
                avail_wp := x :: !avail_wp)
              else (
                if x.level = King then b_k := x;
                avail_bp := x :: !avail_bp))
        board.r5;
      Array.iter
        (fun y ->
          match y with
          | None -> ()
          | Some x ->
              if x.color = White then (
                if x.level = King then w_k := x;
                avail_wp := x :: !avail_wp)
              else (
                if x.level = King then b_k := x;
                avail_bp := x :: !avail_bp))
        board.r6;
      Array.iter
        (fun y ->
          match y with
          | None -> ()
          | Some x ->
              if x.color = White then (
                if x.level = King then w_k := x;
                avail_wp := x :: !avail_wp)
              else (
                if x.level = King then b_k := x;
                avail_bp := x :: !avail_bp))
        board.r7;
      Array.iter
        (fun y ->
          match y with
          | None -> ()
          | Some x ->
              if x.color = White then (
                if x.level = King then w_k := x;
                avail_wp := x :: !avail_wp)
              else (
                if x.level = King then b_k := x;
                avail_bp := x :: !avail_bp))
        board.r8;
      if
        check_piece i_p input o_p true row1 row2 row3 row4 row5 row6
          row7 row8
        = false
      then (
        print_endline
          "Sorry, either this piece doesn't move that way or that's \
           not a valid castling";
        mover_init board)
      else if color_checker i_p o_p input = false then (
        print_endline
          "Sorry, you can't capture one of your own pieces! (or that's \
           not a valid castling, sorry.)";
        mover_init board)
      else if
        clr = White
        && move_into_check
             (Array.get i_pr ic_rel_a)
             input !w_k board.r1 board.r2 board.r3 board.r4 board.r5
             board.r6 board.r7 board.r8
        || clr = Black
           && move_into_check
                (Array.get i_pr ic_rel_a)
                input !b_k board.r1 board.r2 board.r3 board.r4 board.r5
                board.r6 board.r7 board.r8
      then (
        print_endline "Sorry, you can't move into check";
        mover_init board)
      else
        let new_board =
          if o_p <> None && snd (Command.castle i_p input o_p) = false
          then { board with graveyard = rep o_p :: board.graveyard }
          else board
        in
        (* new_board checks if output position is occupied *)
        let moved_piece =
          match Array.get i_pr ic_rel_a with
          | None -> None
          | Some piece ->
              Some
                (Piece.place_piece
                   (Some
                      ( input.[3],
                        int_of_char input.[2] - int_of_char '0' ))
                   (Piece.get_color piece) (Piece.get_level piece)
                   (Piece.get_rep piece) true)
        in
        let moved_piece2 =
          match Array.get o_pr oc_rel_a with
          | None -> None
          | Some piece ->
              Some
                (Piece.place_piece
                   (Piece.get_position piece)
                   (Piece.get_color piece) (Piece.get_level piece)
                   (Piece.get_rep piece) true)
        in
        if snd (Command.castle i_p input o_p) = false then (
          Array.set o_pr oc_rel_a moved_piece;
          Array.set i_pr ic_rel_a None)
        else begin
          Array.set o_pr oc_rel_a None;
          Array.set i_pr ic_rel_a None;
          match fst (Command.castle i_p input o_p) with
          | "ksir" ->
              Array.set o_pr 6 moved_piece;
              Array.set o_pr 5 moved_piece2
          | "qsir" ->
              Array.set o_pr 3 moved_piece2;
              Array.set o_pr 2 moved_piece
          | "ksik" ->
              Array.set o_pr 6 moved_piece2;
              Array.set o_pr 5 moved_piece
          | "qsik" ->
              Array.set o_pr 3 moved_piece;
              Array.set o_pr 2 moved_piece2
          | _ -> failwith "castle function error"
        end;
        if Command.promote_pawn input moved_piece then (
          print_newline ();
          print_endline
            "Congrats! Your pawn has reached the end of the board! \
             What would you like to promote it to?";
          print_endline
            "Your options are Queen, Bishop, Knight, or Rook. Please \
             use the following format:";
          print_endline "Example format: Queen";
          promote_check input moved_piece |> Array.set o_pr oc_rel_a;
          let new_board2 =
            {
              new_board with
              model = update_turn new_board.model Board.Change;
            }
          in
          Display.print_board new_board2;
          print_newline ();
          print_endline "Here are all of the captured pieces:";
          print_list new_board2.graveyard;
          print_newline ();
          mover_init new_board2)
        else
          let new_board2 =
            {
              new_board with
              model = update_turn new_board.model Board.Change;
            }
          in
          Display.print_board new_board2;
          Array.iter
            (fun y ->
              match y with
              | None -> ()
              | Some x ->
                  if x.color = White then (
                    if x.level = King then w_k := x;
                    avail_wp := x :: !avail_wp)
                  else (
                    if x.level = King then b_k := x;
                    avail_bp := x :: !avail_bp))
            new_board2.r1;
          Array.iter
            (fun y ->
              match y with
              | None -> ()
              | Some x ->
                  if x.color = White then (
                    if x.level = King then w_k := x;
                    avail_wp := x :: !avail_wp)
                  else (
                    if x.level = King then b_k := x;
                    avail_bp := x :: !avail_bp))
            new_board2.r2;
          Array.iter
            (fun y ->
              match y with
              | None -> ()
              | Some x ->
                  if x.color = White then (
                    if x.level = King then w_k := x;
                    avail_wp := x :: !avail_wp)
                  else (
                    if x.level = King then b_k := x;
                    avail_bp := x :: !avail_bp))
            new_board2.r3;
          Array.iter
            (fun y ->
              match y with
              | None -> ()
              | Some x ->
                  if x.color = White then (
                    if x.level = King then w_k := x;
                    avail_wp := x :: !avail_wp)
                  else (
                    if x.level = King then b_k := x;
                    avail_bp := x :: !avail_bp))
            new_board2.r4;
          Array.iter
            (fun y ->
              match y with
              | None -> ()
              | Some x ->
                  if x.color = White then (
                    if x.level = King then w_k := x;
                    avail_wp := x :: !avail_wp)
                  else (
                    if x.level = King then b_k := x;
                    avail_bp := x :: !avail_bp))
            new_board2.r5;
          Array.iter
            (fun y ->
              match y with
              | None -> ()
              | Some x ->
                  if x.color = White then (
                    if x.level = King then w_k := x;
                    avail_wp := x :: !avail_wp)
                  else (
                    if x.level = King then b_k := x;
                    avail_bp := x :: !avail_bp))
            new_board2.r6;
          Array.iter
            (fun y ->
              match y with
              | None -> ()
              | Some x ->
                  if x.color = White then (
                    if x.level = King then w_k := x;
                    avail_wp := x :: !avail_wp)
                  else (
                    if x.level = King then b_k := x;
                    avail_bp := x :: !avail_bp))
            new_board2.r7;
          Array.iter
            (fun y ->
              match y with
              | None -> ()
              | Some x ->
                  if x.color = White then (
                    if x.level = King then w_k := x;
                    avail_wp := x :: !avail_wp)
                  else (
                    if x.level = King then b_k := x;
                    avail_bp := x :: !avail_bp))
            new_board2.r8;
          print_newline ();
          print_endline "Here are all of the captured pieces:";
          print_list new_board2.graveyard;
          print_newline ();
          if board.model.turn = White then (
            if
              Command.incheck !avail_wp !b_k false row1 row2 row3 row4
                row5 row6 row7 row8
            then print_endline "Black Player Now In Check!"
            else ();
            if
              Command.has_move !avail_bp row1 row2 row3 row4 row5 row6
                row7 row8
              = []
            then (
              print_endline "Black Player Now In Stalemate!";
              false)
            else if
              Command.checkmated !avail_wp !avail_bp !b_k new_board2.r1
                new_board2.r2 new_board2.r3 new_board2.r4 new_board2.r5
                new_board2.r6 new_board2.r7 new_board2.r8
            then (
              print_endline "Checkmate!";
              false)
            else mover_init new_board2)
          else (
            if
              Command.incheck !avail_bp !w_k false row1 row2 row3 row4
                row5 row6 row7 row8
            then print_endline "White Player Now In Check!"
            else ();
            if
              Command.has_move !avail_wp row1 row2 row3 row4 row5 row6
                row7 row8
              = []
            then (
              print_endline "White Player Now In Stalemate!";
              false)
            else if
              Command.checkmated !avail_bp !avail_wp !w_k new_board2.r1
                new_board2.r2 new_board2.r3 new_board2.r4 new_board2.r5
                new_board2.r6 new_board2.r7 new_board2.r8
            then (
              print_endline "Checkmate!";
              false)
            else mover_init new_board2)

let () = Display.print_board Display.start_board

let _ = mover_init Display.start_board
