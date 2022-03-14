(** TODO: this is the place to parse strings unrelated to main menu -> interact with backend (board, piece, etc) -> passed to frontend (display). There's a lot of placeholders here. *)
open Display
open Board
open Piece

type t = string

exception Quit
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
  | curr::next::[] -> (check_format (explode curr))^(check_format (explode next))
  | _ -> "Incorrect number of letters, please try again."

  let check_quit str = 
    match str with
    | [] -> raise EmptyCommand
    | h::t -> begin 
      match h with
      | "quit" -> if t = [] then raise Quit else "Incorrect command. Did you mean <quit>?"
      | _ -> check_valid_move str
    end

    (* somehow have to pass in color type *)
let parse str = check_quit (String.split_on_char ' ' str |> remove_blank)  
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

(** in larger function, try with when calling check2 *)

let check2 str =
  match str.[1] with 
  |'a' -> (check1 str).c_a
  |'b' -> (check1 str).c_b
  |'c' -> (check1 str).c_c
  |'d' -> (check1 str).c_d
  |'e' -> (check1 str).c_e
  |'f' -> (check1 str).c_f
  |'g' -> (check1 str).c_g
  |'h' -> (check1 str).c_h
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

let check4 str =
  match str.[3] with 
  |'a' -> (check3 str).c_a
  |'b' -> (check3 str).c_b
  |'c' -> (check3 str).c_c
  |'d' -> (check3 str).c_d
  |'e' -> (check3 str).c_e
  |'f' -> (check3 str).c_f
  |'g' -> (check3 str).c_g
  |'h' -> (check3 str).c_h
  | _ -> raise InvalidInput
  
