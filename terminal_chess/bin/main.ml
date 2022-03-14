let in_game = false (* used to toggle between parsing from main_menu and command *)
open Terminal_chess
open Display
open Command

let () = print_board start_board

let matcher str = 
  try Command.check2 (str) with 
  | InvalidInput -> print_endline "Invalid Command, please try again."; 
  try Command.check4 (str) with 
  | InvalidInput -> print_endline "Invalid Command, please try again."; 


let rec request_command s= 
  print_string ("Please enter a move.\n");
  try match Command.parse(read_line()) with
  | x -> matcher x
  with
  | Command.InvalidInput -> print_endline "Invalid Command, please try again.\n"
  | Command.Quit -> print_endline "\nGoodbye \n"; exit 0
  | Command.EmptyCommand -> print_endline "No command found, please try again.\n"
  | Command.InvalidQuit -> print_endline "Incorrect command. Did you mean <quit>?\n";
  request_command s



