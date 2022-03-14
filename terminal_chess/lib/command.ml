(** TODO: this is the place to parse strings unrelated to main menu -> interact with backend (board, piece, etc) -> passed to frontend (display). There's a lot of placeholders here. *)

type t = string

exception Quit

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

