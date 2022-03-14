(** TODO: this is the place to parse strings unrelated to main menu -> interact with backend (board, piece, etc) -> passed to frontend (display). There's a lot of placeholders here. *)

type t = string

exception Quit
exception InvalidInput

 let rec remove_blank str_lst =
  match str_lst with 
  | [] -> []
  | h :: t -> if h = "" then remove_blank t else h :: remove_blank t
    
(* let invalid_move = "Incorrect command. There is more than 4 characters typed. Please do not include + for check, x for take, = for promotion, and ++ or # for checkmate" *)
  (* let check_valid_move str = 
    match str with (* this also implies that str is a list, not a string?*)
    | h::m1::m2::m3::t::[] -> if (h = "O" && m1 = "-" && m2 = "O" && m3 = "-" && t = "O") then "queen side castle" else invalid_move
    | h::m1::t::[] -> if (h = "O" && m1 = "-" && t = "O") then "queen side castling" else invalid_move
    | rank::curr_col::col::row::_ -> "do some more pattern matching. remember promotion and checking whether king will be in check and if next square is movable and not same color"^rank^curr_col^col^row
    | _ -> invalid_move *)

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
  | [] -> "No command found, please try again."
  | curr::next::[] -> (check_format (explode curr))^(check_format (explode next))
  | _ -> "incorrect number of letters"

  let check_quit str = 
    match str with
    | [] -> "No command found, please try again."
    | h::t -> begin 
      match h with
      | "quit" -> if t = [] then raise Quit else "Incorrect command. Did you mean <quit>?"
      | "move" -> check_valid_move t
      | _ -> raise InvalidInput
    end

    (* somehow have to pass in color type *)
  let parse str = 
    check_quit (String.split_on_char ' ' str |> remove_blank)
(* valid str in the form <move (#,letter) (#,letter)> *)
