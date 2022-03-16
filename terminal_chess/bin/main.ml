let in_game = false (* used to toggle between parsing from main_menu and command *)
<<<<<<< HEAD
open Chess
open Piece
open Board
open Display      


exception NoPiece

let rec print_list = function 
| [] -> ()
| e::l -> print_string e ; print_string " " ; print_list l

let rep space =
  match space with 
  | None -> raise NoPiece
  | Some space -> space.rep

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
      (print_endline "Sorry, that's not a valid move. Try again!");
      "wrong move"

and char_in_range str =
  ((Char.code str.[1]) > 104 || (Char.code str.[1]) < 97 ||
    (Char.code str.[3]) > 104 || (Char.code str.[3]) < 97)

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
        let o_p = Array.get o_pr ((Char.code input.[3]) - 97) in (*output piece*)
        let new_board = (if o_p <> None then {board with graveyard = (*rep o_p :: *) board.graveyard} else board) in
        (Array.set o_pr ((Char.code input.[3]) - 97) (Array.get i_pr ((Char.code input.[1]) - 97));
        Array.set i_pr ((Char.code input.[1]) - 97) None);
        (let new_board2 = {new_board with model = update_turn new_board.model Board.Change} in
        Display.print_board new_board2;
        print_newline ();
        print_endline "Here are your captured pieces:";
        print_list new_board2.graveyard;
        print_newline ();
        mover_init new_board2))) end

let () = Display.print_board Display.start_board
let _ = mover_init Display.start_board


=======
open Terminal_chess
open Display
open Command

let rec request_command s= 
  print_string "Please enter a move.\n";
  try match Command.parse(read_line()) with
  | x -> print_endline x (* here is where the new board is returned and where you reprint *)
  with 
  | Command.InvalidInput -> print_endline "Invalid Command, please try again.\n"
  | Command.Quit -> print_endline "\nGoodbye \n"; exit 0
  | Command.EmptyCommand -> print_endline "No command found, please try again.\n"
  | Command.InvalidQuit -> print_endline "Incorrect command. Did you mean <quit>?\n";
  request_command s

let () = print_board start_board;
request_command ""
>>>>>>> bb3f582bd794488e640e73582a84ba4fd9c22424
