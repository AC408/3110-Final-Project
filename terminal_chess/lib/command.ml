 (**TODO: this is the place to parse strings unrelated to main menu -> interact with backend (board, piece, etc) -> passed to frontend (display). There's a lot of placeholders here. *)
open Display

type t = string


exception InvalidInput
exception EmptyCommand
exception InvalidQuit

let rec remove_blank (strlist : string list) =
  match strlist with 
  | [] -> []
  | h :: t -> if h = "" then remove_blank t else h :: remove_blank t 


let rec explode str = 
  match str with
  | "" -> []
  | st -> (String.get str 0)::(explode (String.sub st 1 ((String.length str) -1)))

let check_format str = 
  match str with
  | h::m1::m2::m3::t::[] -> if (h = '(' && m2 = ',' && t = ')') then (Char.escaped m1)^(Char.escaped m3) else raise InvalidInput
  | _ -> raise InvalidInput

let check_valid_move str = 
  match str with
  | [] -> raise EmptyCommand
<<<<<<< HEAD
  | _::curr::next::[] -> (check_format (explode curr))^(check_format (explode next))
  | _ -> raise InvalidInput

let check_quit t = 
    if t = "quit" then true else false

let parse str = String.split_on_char ' ' str |> remove_blank 
=======
  | curr::next::[] -> (check_format (explode curr))^(check_format (explode next))
  | _ -> raise InvalidInput

  let check_quit str = 
    match str with
    | [] -> raise EmptyCommand
    | h::t -> begin 
      match h with
      | "quit" -> if t = [] then raise Quit else raise InvalidQuit
      | "move" -> check_valid_move t
      | _ -> raise InvalidInput
    end
>>>>>>> bb3f582bd794488e640e73582a84ba4fd9c22424

let parse_mod str = String.split_on_char ' ' str |> remove_blank |> check_valid_move

(** abstract out check1 and check3*)
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


  
