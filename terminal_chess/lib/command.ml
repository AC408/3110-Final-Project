(** TODO: this is the place to parse strings unrelated to main menu -> interact with backend (board, piece, etc) -> passed to frontend (display). There's a lot of placeholders here. *)
open Terminal_chess
open Display
open Board
open Piece

type t = string

exception Quit
exception InputProblem

(* let rec remove_blank (str : string) =
  let strlist = [str] in
  match strlist with 
  | [] -> []
  | h :: t -> if h = "" then remove_blank else h :: remove_blank t 
  (*won't this yield a list? edited the beginning, not sure what to do about the end if we want a string*)
  feel free to go back to original if this train of thought doesn't make sense *)

(* let invalid_move = "Incorrect command. There is more than 4 characters typed. Please do not include + for check, x for take, = for promotion, and ++ or # for checkmate"
  let check_valid_move str = 
    match str with (* this also implies that str is a list, not a string?*)
    | h::m1::m2::m3::t::[] -> if (h = "O" && m1 = "-" && m2 = "O" && m3 = "-" && t = "O") then "queen side castle" else invalid_move
    | h::m1::t::[] -> if (h = "O" && m1 = "-" && t = "O") then "queen side castling" else invalid_move
    | rank::curr_col::col::row::_ -> "do some more pattern matching. remember promotion and checking whether king will be in check and if next square is movable and not same color"^rank^curr_col^col^row
    | _ -> invalid_move

  let check_quit str = 
    match str with
    | [] -> "No command found, please try again."
    | h::t -> begin 
      match h with
      | "quit" -> if t = [] then raise Quit else "Incorrect command. Did you mean <quit>?"
      | _ -> check_valid_move str
    end

    (* somehow have to pass in color type *)
  let parse str = 
    check_quit (String.split_on_char ' ' str |> remove_blank) *)
  
let check1 str =
  match str.[0] with
  | '1' -> row1
  | '2' -> row2
  | '3' -> row3
  | '4' -> row4
  | '5' -> row5
  | '6' -> row6
  | '7' -> row7
  | '8' -> row8
  | _ -> raise InputProblem

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
  | _ -> raise InputProblem


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
  | _ -> raise InputProblem

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
  | _ -> raise InputProblem
  
