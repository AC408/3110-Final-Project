 (**TODO: this is the place to parse strings unrelated to main menu -> interact with backend (board, piece, etc) -> passed to frontend (display). There's a lot of placeholders here. *)
open Display
open Piece

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

let get_upper_col str = 
  if (Char.code str.[3] > Char.code str.[1]) then (Char.code str.[3] - Char.code 'a') else (Char.code str.[1] - Char.code 'a')

let get_lower_col str = 
  if (Char.code str.[3] > Char.code str.[1]) then (Char.code str.[1] - Char.code 'a') else (Char.code str.[3] - Char.code 'a')
    
let rec go_left lower_col upper_col row = 
  if lower_col = (upper_col-1) || lower_col = upper_col then true else 
  if Array.get row (upper_col-1) <> None then false
  else go_left lower_col (upper_col-1) row
  
let rec go_down str gate gate2= 
  if(gate) then
    if (int_of_char str.[2] = (int_of_char str.[0])-1) || (int_of_char str.[2] = (int_of_char str.[0])+1) then true else go_down str false true
  else if (int_of_char str.[2] = int_of_char str.[0]) then true else
    if(gate2) then
let new_str = str |> explode in 
        match new_str with 
        | row1::col1::row2::col2::[] -> 
          if (row2 < row1) then go_down ((Char.escaped row1)^(Char.escaped col1)^(string_of_int((Char.code row2 - Char.code '0') +1))^(Char.escaped col2)) (false)  false
          else go_down ((Char.escaped row1)^(Char.escaped col1)^(string_of_int((Char.code row2 - Char.code '0') -1))^(Char.escaped col2)) (false)false
        | _  -> false
  
        else if Array.get (check3 str) (Char.code str.[3] - Char.code 'a') <> None then false
    else let new_str = str |> explode in 
      match new_str with 
      | row1::col1::row2::col2::[] -> 
        if (row2 < row1) then go_down ((Char.escaped row1)^(Char.escaped col1)^(string_of_int((Char.code row2 - Char.code '0') +1))^(Char.escaped col2)) (false) false
        else go_down ((Char.escaped row1)^(Char.escaped col1)^(string_of_int((Char.code row2 - Char.code '0') -1))^(Char.escaped col2)) (false)false
      | _  -> false

let rec go_diagonal direction str gate gate2 =
  let diagonal_direction = if(direction = "up_right") then 1 else -1 in
  let row_diff = int_of_char str.[2] - int_of_char str.[0] in
  let col_diff = Char.code str.[3] - Char.code str.[1] in
  if(gate) then
    if (abs(row_diff) = 1 || abs(col_diff) = 1) then true else go_diagonal direction str false true
  else if (abs(row_diff) <= 1 || abs(col_diff) <= 1) then true
  else if ((Array.get (check3 str) (Char.code str.[3] - Char.code 'a') <> None) && (gate2 == false)) then false
  else let new_str = str |> explode in 
      match new_str with 
      | row1::col1::row2::col2::[] -> 
        if (row2 < row1) then go_diagonal direction ((Char.escaped row1)^(Char.escaped col1)^(string_of_int((Char.code row2 - Char.code '0') +1))^(Char.escaped (Char.chr(int_of_char col2 +(diagonal_direction))))) (false)false
        else go_diagonal direction ((Char.escaped row1)^(Char.escaped col1)^(string_of_int((Char.code row2 - Char.code '0') -1))^(Char.escaped (Char.chr(int_of_char col2 +(diagonal_direction))))) (false)false
      | _  -> false

      let check_horizontal str = 
  (int_of_char (str.[2]) - int_of_char (str.[0]) = 0) && (go_left (get_lower_col str) (get_upper_col str) (check1 str))

let check_vertical str = 
  (Char.code str.[3] - Char.code str.[1] = 0) && (go_down str true) true

let check_diagonal str = 
  if(abs(int_of_char str.[2] - int_of_char str.[0]) = abs (Char.code str.[3] - Char.code str.[1])) <> true then false else 
  if((int_of_char str.[2] > int_of_char str.[0]) && (Char.code str.[3] > Char.code str.[1])) then
    (go_diagonal "up_right" str true) true
  else if((int_of_char str.[2] > int_of_char str.[0]) && (Char.code str.[3] < Char.code str.[1])) then 
      (go_diagonal "up_left" str true) true
  else if((int_of_char str.[2] < int_of_char str.[0]) && (Char.code str.[3] > Char.code str.[1])) then 
    (go_diagonal "up_left" str true) true
  else (go_diagonal "up_right" str true) true

let rook_check input = check_vertical input || check_horizontal input

let bishop_check input = check_diagonal input

let queen_check input = check_diagonal input || check_horizontal input || check_vertical input

let king_check input = (check_horizontal input && abs(Char.code input.[3] - Char.code input.[1]) = 1) || 
(check_vertical input && abs (int_of_char input.[2] - int_of_char input.[1]) = 1) || 
(check_diagonal input && (abs(Char.code(input.[3]) - Char.code(input.[1])) = 1) || 
(abs(int_of_char input.[2] - int_of_char input.[0]) = 1))

let knight_check input = ((abs(Char.code input.[3]-Char.code input.[1]) = 2) && 
abs(int_of_char input.[2] - int_of_char input.[0]) = 1) || 
((abs(int_of_char input.[2] - int_of_char input.[0]) = 2) && 
abs(Char.code input.[3] - Char.code input.[1]) = 1)

let pawn_check input moved color = 
  let sign = if color = White then 1 else -1 in
  if (int_of_char input.[2] - int_of_char input.[0] = 1*sign && 
    abs(Char.code input.[3] - Char.code input.[1]) <= 1) then 
      let get_element = Array.get (check3 input) ((Char.code input.[3]) - 97) in 
      match get_element with
      | None -> check_vertical input (* this means that the element is none -> can't go diagonally *)
      | Some _ -> (check_diagonal input) 
  else if ((int_of_char input.[2] - int_of_char input.[0] = 2*sign) && (Char.code input.[3] = Char.code input.[1])) && (moved = false) then
    let get_elt = Array.get (check3 input) ((Char.code input.[3]) - 97) in
    match get_elt with
    | None -> true
    | Some _ -> false
  else false

let castle i_p o_p =
  match i_p, o_p with
  | Some i_p, Some o_p -> 
    if get_level i_p = King then (
      if get_level o_p = Rook then (
        if (have_moved i_p || have_moved o_p) then false
        else true
      )
      else false
      )
    else if get_level i_p = Rook then (
      if get_level o_p = King then (
        if (have_moved i_p || have_moved o_p) then false
        else true
      )
      else false
    )
    else false
  | None, _ -> false
  | _, None -> false

  let check_piece ipc str opc =
    match ipc with
    | None -> false
    | Some pc -> (
    if get_level pc = Pawn then 
      pawn_check str pc.moved (get_color pc)
    else if get_level pc = Rook then rook_check str
    else if get_level pc = Bishop then bishop_check str
    else if get_level pc = Knight then knight_check str
    else if get_level pc = Queen then queen_check str
    else king_check str) || (castle ipc opc)

  let color_checker i_p o_p =
    match o_p with 
    | None -> true
    | Some op -> 
      match i_p with 
      | None -> false
      | Some ip ->
      if (castle i_p o_p = false && get_color op = get_color ip) then false
      else if (castle i_p o_p = true && get_color op <> get_color ip) then false
      else true 