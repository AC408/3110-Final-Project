let in_game = false (* used to toggle between parsing from main_menu and command *)

open Terminal_chess
open Display

let () = print_board start_board;
print_string "please enter a move\n";
try match Command.parse(read_line()) with
| x -> print_endline x
with 
| Command.InvalidInput -> print_endline "invalid command"
| Command.Quit -> print_endline "\n goodbye \n"; exit 0