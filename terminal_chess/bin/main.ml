let in_game = false (* used to toggle between parsing from main_menu and command *)
open Chess
open Piece
open Board
open Display
open Command      


let rec print_list = function 
| [] -> ()
| e::l -> print_string e ; print_string " " ; print_list l

let color_matcher board = function
  | None -> raise NoPiece
  | Some space -> board.model.turn <> (space).color

let rec filled_input board space =
  try color_matcher board space with
  | NoPiece -> 
    print_endline "Sorry, there's no piece there. Check your input and try again.";
    mover_init board

and wrong_input str =
    try Command.parse_mod str with 
    | Command.InvalidInput -> 
      (print_endline "Sorry, you haven't entered a valid move. Check your formatting!");
      "wrong move"

and char_in_range str =
  ((Char.code str.[1]) > Char.code 'h' || (Char.code str.[1]) < Char.code 'a' ||
    (Char.code str.[3]) > Char.code 'h' || (Char.code str.[3]) < Char.code 'a')

and mover_init board =
  print_newline ();
  print_endline "Please enter a move. Example format: move (3,b) (4,c) ";
  let in_command = read_line () in
  if Command.check_quit (in_command) then Stdlib.exit 0
  else 
  if wrong_input (in_command) = "wrong move" then mover_init board
  else 
    begin
    if List.hd (Command.parse (in_command)) <> "move" || char_in_range (Command.parse_mod (in_command))
    then 
      ((print_endline "Sorry, that doesn't work. Check your input and try again.");
      mover_init board)
    else 
      if wrong_input (in_command) = "wrong move" then mover_init board
      else 
      let input = Command.parse_mod (in_command) in
      if (filled_input board (Array.get (Command.check1 input) ((Char.code input.[1]) - 97)))
      then 
        (print_endline "Sorry, it's not your turn. Try again later!.";
        mover_init board)
      else 
        ((let input = Command.parse_mod (in_command) in 
        let i_pr = Command.check1 input in (*input piecerow*)
        let o_pr = Command.check3 input in (*output piecerow*)
        let i_p = Array.get i_pr ((Char.code input.[1]) - 97) in (*input piece*)
        let o_p = Array.get o_pr ((Char.code input.[3]) - 97) in (*output piece*)
        if check_piece i_p input o_p = false then (
          (print_endline "Sorry, either this piece doesn't move that way or that's not a valid castling");
          mover_init board )
        else if color_checker i_p o_p input = false then (
          (print_endline "Sorry, you can't capture one of your own pieces! (or that's not a valid castling, sorry.)");
          mover_init board )
        else 
          let new_board = (if (o_p <> None && Command.castle i_p input o_p = false) then {board with graveyard = rep o_p ::  board.graveyard} else board) in (* new_board checks if output position is occupied *)
          let moved_piece = 
            match (Array.get i_pr ((Char.code input.[1]) - 97)) with
            | None -> None
            | Some piece -> Some (Piece.place_piece (Piece.get_position piece) (Piece.get_color piece) (Piece.get_level piece) (Piece.get_rep piece) true)
          in
          let moved_piece2 =
            match (Array.get o_pr ((Char.code input.[3]) - 97)) with
            | None -> None
            | Some piece -> Some (Piece.place_piece (Piece.get_position piece) (Piece.get_color piece) (Piece.get_level piece) (Piece.get_rep piece) true)
          in
          (Array.set o_pr ((Char.code input.[3]) - 97) moved_piece;
          if Command.castle i_p input o_p = false then
          Array.set i_pr ((Char.code input.[1]) - 97) None
          else Array.set i_pr ((Char.code input.[1]) - 97) moved_piece2);
          (let new_board2 = {new_board with model = update_turn new_board.model Board.Change} in
          Display.print_board new_board2;
          print_newline ();
          print_endline "Here are your captured pieces:";
          print_list new_board2.graveyard;
          print_newline ();
          mover_init new_board2))) end

let () = Display.print_board Display.start_board
let _ = mover_init Display.start_board

 
