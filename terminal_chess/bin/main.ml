let in_game = false
(*toggle between parsing from main menu and command*)

open Chess
open Piece
open Board
open Display
open Command

let rec print_avail_p lst =
  match lst with
  | [] -> ""
  | h :: t -> h.rep ^ print_avail_p t

let rec print_list = function
  | [] -> ()
  | e :: l ->
      print_string e;
      print_string " ";
      print_list l

let color_matcher board = function
  | None -> raise NoPiece
  | Some space -> board.model.turn <> space.color

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
  print_endline "Please enter a move. Example format: move (3,b) (4,c) ";
  let in_command = read_line () in
  if Command.check_quit in_command then Stdlib.exit 0
  else if wrong_input in_command = "wrong move" then mover_init board
  else if
    List.hd (Command.parse in_command) <> "move"
    || char_in_range (Command.parse_mod in_command)
  then (
    print_endline
      "Sorry, that doesn't work. Check your input and try again.";
    mover_init board)
  else if wrong_input in_command = "wrong move" then mover_init board
  else
    let input = Command.parse_mod in_command in
    if
      filled_input board
        (Array.get (Command.check1 input) (Char.code input.[1] - 97))
    then (
      print_endline "Sorry, it's not your turn. Try again later!.";
      mover_init board)
    else
      let input = Command.parse_mod in_command in
      let i_pr = Command.check1 input in
      (*input piecerow*)
      let o_pr = Command.check3 input in
      (*output piecerow*)
      let i_p = Array.get i_pr (Char.code input.[1] - 97) in
      (*ip*)
      let o_p = Array.get o_pr (Char.code input.[3] - 97) in
      (*op*)
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
      else
        let new_board =
          if o_p <> None && Command.castle i_p input o_p = false then
            { board with graveyard = rep o_p :: board.graveyard }
          else board
        in
        (* new_board checks if output position is occupied *)
        let moved_piece =
          match Array.get i_pr (Char.code input.[1] - 97) with
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
          match Array.get o_pr (Char.code input.[3] - 97) with
          | None -> None
          | Some piece ->
              Some
                (Piece.place_piece
                   (Piece.get_position piece)
                   (Piece.get_color piece) (Piece.get_level piece)
                   (Piece.get_rep piece) true)
        in
        Array.set o_pr (Char.code input.[3] - 97) moved_piece;
        if Command.castle i_p input o_p = false then
          Array.set i_pr (Char.code input.[1] - 97) None
        else Array.set i_pr (Char.code input.[1] - 97) moved_piece2;
        if Command.promote_pawn input moved_piece then (
          print_newline ();
          print_endline
            "Congrats! Your pawn has reached the end of the board! \
             What would you like to promote it to?";
          print_endline
            "Your options are Queen, Bishop, Knight, or Rook. Please \
             use the following format:";
          print_endline "Example format: Queen";
          Array.set o_pr
            (Char.code input.[3] - 97)
            (promote_check input moved_piece);
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
                  if x.color = White then avail_wp := x :: !avail_wp
                  else avail_bp := x :: !avail_bp;
                  if x = whiteking then w_k := x
                  else if x = blackking then b_k := x)
            new_board2.r1;
          Array.iter
            (fun y ->
              match y with
              | None -> ()
              | Some x ->
                  if x.color = White then avail_wp := x :: !avail_wp
                  else avail_bp := x :: !avail_bp;
                  if x = whiteking then w_k := x
                  else if x = blackking then b_k := x)
            new_board2.r2;
          Array.iter
            (fun y ->
              match y with
              | None -> ()
              | Some x ->
                  if x.color = White then avail_wp := x :: !avail_wp
                  else avail_bp := x :: !avail_bp;
                  if x = whiteking then w_k := x
                  else if x = blackking then b_k := x)
            new_board2.r3;
          Array.iter
            (fun y ->
              match y with
              | None -> ()
              | Some x ->
                  if x.color = White then avail_wp := x :: !avail_wp
                  else avail_bp := x :: !avail_bp;
                  if x = whiteking then w_k := x
                  else if x = blackking then b_k := x)
            new_board2.r4;
          Array.iter
            (fun y ->
              match y with
              | None -> ()
              | Some x ->
                  if x.color = White then avail_wp := x :: !avail_wp
                  else avail_bp := x :: !avail_bp;
                  if x = whiteking then w_k := x
                  else if x = blackking then b_k := x)
            new_board2.r5;
          Array.iter
            (fun y ->
              match y with
              | None -> ()
              | Some x ->
                  if x.color = White then avail_wp := x :: !avail_wp
                  else avail_bp := x :: !avail_bp;
                  if x = whiteking then w_k := x
                  else if x = blackking then b_k := x)
            new_board2.r6;
          Array.iter
            (fun y ->
              match y with
              | None -> ()
              | Some x ->
                  if x.color = White then avail_wp := x :: !avail_wp
                  else avail_bp := x :: !avail_bp;
                  if x = whiteking then w_k := x
                  else if x = blackking then b_k := x)
            new_board2.r7;
          Array.iter
            (fun y ->
              match y with
              | None -> ()
              | Some x ->
                  if x.color = White then avail_wp := x :: !avail_wp
                  else avail_bp := x :: !avail_bp;
                  if x = whiteking then w_k := x
                  else if x = blackking then b_k := x)
            new_board2.r8;
          print_newline ();
          print_endline "Here are your captured pieces:";
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
            then print_endline "Black Player Now In Stalemate!"
            else ();
            if
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
            then print_endline "White Player Now In Stalemate!"
            else ();
            if
              Command.checkmated !avail_bp !avail_wp !w_k new_board2.r1
                new_board2.r2 new_board2.r3 new_board2.r4 new_board2.r5
                new_board2.r6 new_board2.r7 new_board2.r8
            then (
              print_endline "Checkmate!";
              false)
            else mover_init new_board2)

let () = Display.print_board Display.start_board

let _ = mover_init Display.start_board
