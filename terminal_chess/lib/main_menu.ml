(** TODO: There's a lot of placeholders here. *)

type t = string

exception Quit

let pawn_rules =
  "Pawns can only move one step forward to the square in front of it. \
   It may move one step diagonally to the square on the left or right \
   of the square in front of it if "

let rec remove_blank str =
  match str with
  | [] -> []
  | h :: t -> if h = "" then remove_blank t else h :: remove_blank t

(* let rec process_command s = if s = "main" then begin print_string
   "\nWelcome to Terminal Chess. This is the main menu. Type <play> to
   play and <rules> for rules \n"; match read_line () with | "play" ->
   print_endline "play" | "move_set" -> process_command "move_set" |
   "rules" -> print_endline "Type <next> to navigate to the 2nd rule
   page and <prev> to navigate to the 1st rule page. Type <menu> to
   return to the main menu \n"; process_command s; | _ -> print_endline
   "invalid entry"; process_command s end else if s = "move_set" then
   begin print_endline "A move is a valid move if it does not put your
   own king in check and it moves to a valid square. A square is valid
   if it does not have a piece of your own color. If you cannot make a
   move without putting your king in check, it is considered a
   stalemate. Type <next> to navigate to the 2nd moveset page and <prev>
   to navigate to the 1st moveset page. Type <menu> to return to the
   main \n"; match read_line () with | "prev" -> print_endline "first
   page of move_set"; process_command "move_set" | "next" ->
   print_endline "next_page of move_set"; process_command "move_set" | _
   -> print_endline "invalid entry"; process_command "move_set" end else
   print_endline "invalid state" *)

let check_quit str =
  match str with
  | [] -> "No command found, please try again."
  | h :: t -> begin
      match h with
      | "quit" ->
          if t = [] then raise Quit
          else "Incorrect command. Did you mean <quit>?"
      | _ -> pawn_rules
    end

(* somehow have to pass in color type *)
let parse str = check_quit (String.split_on_char ' ' str |> remove_blank)
