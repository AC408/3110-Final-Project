open Chess
open Piece
open Board
open Display
open Command

let move_into_check piece cmd king grid =
  let p =
    match piece with
    | None -> failwith "input not a piece"
    | Some pc -> pc
  in
  let k = ref king in
  let a_bp = ref [] in
  let a_wp = ref [] in
  let copy_grid = Array.map Array.copy grid in
  let o_r = Command.check3 cmd copy_grid in
  let new_pos = Some (cmd.[3], int_of_char cmd.[2] - int_of_char '0') in
  let moved_piece = { p with position = new_pos; moved = true } in
  if p = king then k := moved_piece;
  Some moved_piece |> Array.set o_r (Char.code cmd.[3] - Char.code 'a');
  None
  |> Array.set
       (Command.check1 cmd copy_grid)
       (Char.code cmd.[1] - Char.code 'a');
  Command.update_avail_lst a_wp a_wp copy_grid;
  Command.incheck !a_wp !a_bp !k copy_grid

let make_new_promoted_p piece =
  let color = Piece.get_color piece in
  let new_p =
    match read_line () with
    | "Queen" -> if color = White then whitequeen else blackqueen
    | "Bishop" -> if color = White then whitebishop1 else blackbishop1
    | "Knight" -> if color = White then whiteknight1 else blackknight1
    | "Rook" -> if color = White then whiterook1 else blackrook1
    | _ ->
        failwith
          "Sorry, that's not a valid rank to promote your pawn to. Try \
           again!"
  in
  Some
    {
      piece with
      level = Piece.get_level new_p;
      rep = Piece.get_rep new_p;
      moved = true;
    }

let rec promote_check piece =
  try
    match piece with
    | None ->
        print_endline
          "Sorry, you can't promote an empty space. Try again!";
        promote_check piece
    | Some p -> make_new_promoted_p p
  with
  | Failure x ->
      print_endline x;
      promote_check piece

let rec filled_input board space =
  try Command.color_matcher board space with
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
      ic_rel_a
      |> Array.get (Command.check1 input board.grid)
      |> filled_input board
    then (
      print_endline "Sorry, it's not your turn. Try again later!.";
      mover_init board)
    else
      let input = Command.parse_mod in_command in
      let i_pr = Command.check1 input board.grid in
      (*input piecerow*)
      let o_pr = Command.check3 input board.grid in
      (*output piecerow*)
      let i_p = Array.get i_pr ic_rel_a in
      (*ip*)
      let o_p = Array.get o_pr oc_rel_a in
      (*op*)
      let clr = board.model.turn in
      if check_piece i_p input o_p board.grid = false then (
        print_endline
          "Sorry, either this piece doesn't move that way or that's \
           not a valid castling";
        mover_init board)
      else if color_checker i_p o_p input board.grid = false then (
        print_endline
          "Sorry, you can't capture one of your own pieces! (or that's \
           not a valid castling, sorry.)";
        mover_init board)
      else if
        (clr = White && move_into_check i_p input !Display.wk board.grid)
        || clr = Black
           && move_into_check i_p input !Display.wk board.grid
      then (
        print_endline
          "Sorry, you can't move into check or stay in check";
        mover_init board)
      else
        let new_board =
          if
            o_p <> None
            && snd (Command.castle i_p input o_p board.grid) = false
          then { board with graveyard = rep o_p :: board.graveyard }
          else board
        in
        (* new_board checks if output position is occupied *)
        let moved_piece =
          match Array.get i_pr ic_rel_a with
          | None -> None
          | Some piece ->
              let new_p =
                Piece.place_piece
                  (Some
                     (input.[3], int_of_char input.[2] - int_of_char '0'))
                  (Piece.get_color piece) (Piece.get_level piece)
                  (Piece.get_rep piece) true
              in
              if new_p.level = King then
                if new_p.color = White then Display.wk := new_p
                else Display.bk := new_p;
              Some new_p
        in
        let moved_piece2 =
          match Array.get o_pr oc_rel_a with
          | None -> None
          | Some piece ->
              let new_p =
                Piece.place_piece
                  (Piece.get_position piece)
                  (Piece.get_color piece) (Piece.get_level piece)
                  (Piece.get_rep piece) true
              in
              if new_p.level = King then
                if new_p.color = White then Display.wk := new_p
                else Display.bk := new_p;
              Some new_p
        in
        if snd (Command.castle i_p input o_p board.grid) = false then (
          Array.set o_pr oc_rel_a moved_piece;
          Array.set i_pr ic_rel_a None)
        else begin
          Array.set o_pr oc_rel_a None;
          Array.set i_pr ic_rel_a None;
          match fst (Command.castle i_p input o_p board.grid) with
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
          promote_check moved_piece |> Array.set o_pr oc_rel_a;
          let new_board2 =
            {
              new_board with
              model = update_turn new_board.model Board.Change;
            }
          in
          Display.print_board new_board2.grid;
          print_newline ();
          print_endline "Here are all of the captured pieces:";
          Command.print_list new_board2.graveyard;
          print_newline ();
          mover_init new_board2)
        else
          let new_board2 =
            {
              new_board with
              model = update_turn new_board.model Board.Change;
            }
          in
          Display.print_board new_board2.grid;
          Command.update_avail_lst avail_wp avail_bp new_board2.grid;
          print_newline ();
          print_endline "Here are all of the captured pieces:";
          Command.print_list new_board2.graveyard;
          print_newline ();
          if board.model.turn = White then (
            if Command.incheck !avail_wp !avail_bp !bk board.grid then
              print_endline "Black Player Now In Check!"
            else ();
            if Command.has_move !avail_wp !avail_bp !bk board.grid = []
            then (
              print_endline "Black Player Now In Stalemate!";
              false)
            else if
              Command.checkmated !avail_wp !avail_bp !Display.bk
                new_board2.grid
            then (
              print_endline "Checkmate!";
              false)
            else mover_init new_board2)
          else (
            if Command.incheck !avail_wp !avail_bp !wk board.grid then
              print_endline "White Player Now In Check!"
            else ();
            if Command.has_move !avail_wp !avail_bp !wk board.grid = []
            then (
              print_endline "White Player Now In Stalemate!";
              false)
            else if
              Command.checkmated !avail_wp !avail_bp !Display.wk
                new_board2.grid
            then (
              print_endline "Checkmate!";
              false)
            else mover_init new_board2)

let () = Display.print_board Display.start_board.grid

let _ = mover_init Display.start_board
