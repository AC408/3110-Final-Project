 (**TODO: this is the place to parse strings unrelated to main menu -> interact with backend (board, piece, etc) -> passed to frontend (display). There's a lot of placeholders here. *)
open Display

type t = string


exception InvalidInput
exception EmptyCommand
exception InvalidQuit

(* given a list of string, removes empty string element *)
let rec remove_blank (strlist : string list) =
  match strlist with 
  | [] -> []
  | h :: t -> if h = "" then remove_blank t else h :: remove_blank t 

(* given a string, returns a list of chars equivalent to string *)
let rec explode str = 
  match str with
  | "" -> []
  | st -> (String.get str 0)::(explode (String.sub st 1 ((String.length str) -1)))

(* checks whether the 2 element from check_valid_move is in the format (a,b) and returns ab *)
  let check_format str = 
  match str with
  | h::m1::m2::m3::t::[] -> if (h = '(' && m2 = ',' && t = ')') then (Char.escaped m1)^(Char.escaped m3) else raise InvalidInput
  | _ -> raise InvalidInput

(* if string list started with move, check whether it has 2 more element for curr pos and next pos. 
  Returns the 4 letter representation of a move from concating 2 2 letter strings. Ex, (2,b) (3,b) -> 2b3b *)
  let check_valid_move str = 
  match str with
  | [] -> raise EmptyCommand
  | _::curr::next::[] -> (check_format (explode curr))^(check_format (explode next))
  | _ -> raise InvalidInput

(* returns true if the string list starts with "quit" else false *)
  let check_quit t = 
    if t = "quit" then true else false

(* The string is split based on empty space and all empty space removed*)
let parse str = String.split_on_char ' ' str |> remove_blank 

(* The string is split based on empty space and all empty space removed and then checked to see if it is a valid move*)
let parse_mod str = parse str |> check_valid_move

(*This is the row that corresponds to the input piece that the user selects*)
let check1 (str : string) =
  match str.[0] with
  | '1' -> row1
  | '2' -> row2
  | '3' -> row3
  | '4' -> row4
  | '5' -> row5
  | '6' -> row6
  | '7' -> row7
  | '8' -> row8
  | _ -> raise InvalidInput

(*This is the row that corresponds to the output space that the user selects*)
let check3 str =
  match str.[2] with
  | '1' ->  row1
  | '2' ->  row2
  | '3' ->  row3
  | '4' ->  row4
  | '5' ->  row5
  | '6' ->  row6
  | '7' ->  row7
  | '8' ->  row8
  | _ -> raise InvalidInput

let check_horizontal str = 
  int_of_char (str.[3]) - int_of_char (str.[1]) = 0

let check_vertical str = 
  Char.code str.[2] - Char.code str.[0] = 0

let check_diagonal str = 
int_of_char str.[3] - int_of_char str.[1] = Char.code str.[2] - Char.code str.[0] || int_of_char str.[3] - int_of_char str.[1] = - (Char.code str.[2] - Char.code str.[0])

let rook_check input = check_vertical input || check_horizontal input

let bishop_check input = check_diagonal input

let queen_check input = check_diagonal input || check_horizontal input || check_vertical input

let king_check input = (check_horizontal input && (Char.code input.[2] - Char.code input.[0] = 1)) || (check_vertical input && int_of_char input.[3] - int_of_char input.[0] = 1))

abs