open Display
(**TODO: this is the place to parse strings unrelated to main menu ->
   interact with backend (board, piece, etc) -> passed to frontend
   (display). There's a lot of placeholders here. *)

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
  | st ->
      String.get str 0
      :: (String.length str - 1 |> String.sub st 1 |> explode)

(* checks whether the 2 element from check_valid_move is in the format
   (a,b) and returns ab *)
let check_format str =
  match str with
  | [ h; m1; m2; m3; t ] ->
      if h = '(' && m2 = ',' && t = ')' then
        Char.escaped m1 ^ Char.escaped m3
      else raise InvalidInput
  | _ -> raise InvalidInput

(* if string list started with move, check whether it has 2 more element
   for curr pos and next pos. Returns the 4 letter representation of a
   move from concating 2 2 letter strings. Ex, (2,b) (3,b) -> 2b3b *)
let check_valid_move str =
  match str with
  | [] -> raise EmptyCommand
  | [ _; curr; next ] ->
      explode curr |> check_format
      |> ( ^ ) (explode next |> check_format)
  | _ -> raise InvalidInput

(* returns true if the string list starts with "quit" else false *)
let check_quit t = if t = "quit" then true else false

(* The string is split based on empty space and all empty space
   removed*)
let parse str = String.split_on_char ' ' str |> remove_blank

(* The string is split based on empty space and all empty space removed
   and then checked to see if it is a valid move*)
let parse_mod str = parse str |> check_valid_move

(*This is the row that corresponds to the input piece that the user
  selects*)
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

(*This is the row that corresponds to the output space that the user
  selects*)
let check3 str =
  match str.[2] with
  | '1' -> row1
  | '2' -> row2
  | '3' -> row3
  | '4' -> row4
  | '5' -> row5
  | '6' -> row6
  | '7' -> row7
  | '8' -> row8
  | _ -> raise InvalidInput

let checkn3 (str : string) r1 r2 r3 r4 r5 r6 r7 r8 =
  match str.[2] with
  | '1' -> r1
  | '2' -> r2
  | '3' -> r3
  | '4' -> r4
  | '5' -> r5
  | '6' -> r6
  | '7' -> r7
  | '8' -> r8
  | _ -> raise InvalidInput

let rec go_left lower_col upper_col row =
  if lower_col = upper_col - 1 || lower_col = upper_col then true
  else if Array.get row (upper_col - 1) <> None then false
  else go_left lower_col (upper_col - 1) row

let rec go_down str gate gate2 is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  let row_diff = int_of_char str.[2] - int_of_char str.[0] in
  if gate then
    if row_diff |> abs = 1 then true
    else go_down str false true is_sim r1 r2 r3 r4 r5 r6 r7 r8
  else if row_diff = 0 then true
  else if gate2 then
    let new_str = str |> explode in
    match new_str with
    | [ row1; col1; row2; col2 ] ->
        if row2 < row1 then
          go_down
            (Char.escaped row1 ^ Char.escaped col1
            ^ string_of_int (Char.code row2 - Char.code '0' + 1)
            ^ Char.escaped col2)
            false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
        else
          go_down
            (Char.escaped row1 ^ Char.escaped col1
            ^ string_of_int (Char.code row2 - Char.code '0' - 1)
            ^ Char.escaped col2)
            false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
    | _ -> false
  else if
    let checkarr =
      if is_sim then
        Array.get
          (checkn3 str r1 r2 r3 r4 r5 r6 r7 r8)
          (Char.code str.[3] - Char.code 'a')
      else Array.get (check3 str) (Char.code str.[3] - Char.code 'a')
    in
    checkarr <> None
  then false
  else
    let new_str = str |> explode in
    match new_str with
    | [ row1; col1; row2; col2 ] ->
        if row2 < row1 then
          go_down
            (Char.escaped row1 ^ Char.escaped col1
            ^ string_of_int (Char.code row2 - Char.code '0' + 1)
            ^ Char.escaped col2)
            false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
        else
          go_down
            (Char.escaped row1 ^ Char.escaped col1
            ^ string_of_int (Char.code row2 - Char.code '0' - 1)
            ^ Char.escaped col2)
            false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
    | _ -> false

let rec go_diagonal
    direction
    str
    gate
    gate2
    is_sim
    r1
    r2
    r3
    r4
    r5
    r6
    r7
    r8 =
  let diagonal_direction = if direction = "up_right" then 1 else -1 in
  let row_diff = int_of_char str.[2] - int_of_char str.[0] in
  let col_diff = Char.code str.[3] - Char.code str.[1] in
  let check_fun =
    if is_sim then checkn3 str r1 r2 r3 r4 r5 r6 r7 r8 else check3 str
  in
  if gate then
    if abs row_diff = 1 || abs col_diff = 1 then true
    else
      go_diagonal direction str false true is_sim r1 r2 r3 r4 r5 r6 r7
        r8
  else if abs row_diff <= 1 || abs col_diff <= 1 then true
  else if
    Char.code str.[3] - Char.code 'a' |> Array.get check_fun <> None
    && gate2 == false
  then false
  else
    let new_str = str |> explode in
    match new_str with
    | [ row1; col1; row2; col2 ] ->
        if row2 < row1 then
          go_diagonal direction
            (Char.escaped row1 ^ Char.escaped col1
            ^ (Char.code row2 - Char.code '0' + 1 |> string_of_int)
            ^ (int_of_char col2 + diagonal_direction
              |> Char.chr |> Char.escaped))
            false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
        else
          go_diagonal direction
            (Char.escaped row1 ^ Char.escaped col1
            ^ (Char.code row2 - Char.code '0' - 1 |> string_of_int)
            ^ (int_of_char col2 + diagonal_direction
              |> Char.chr |> Char.escaped))
            false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
    | _ -> false

let check_horizontal str =
  let int_a = Char.code 'a' in
  let o_gt_i = Char.code str.[3] > Char.code str.[1] in
  let get_upper_col =
    if o_gt_i then Char.code str.[3] - int_a
    else Char.code str.[1] - int_a
  in
  let get_lower_col =
    if o_gt_i then Char.code str.[1] - int_a
    else Char.code str.[3] - int_a
  in
  int_of_char str.[2] - int_of_char str.[0] = 0
  && go_left get_lower_col get_upper_col (check1 str)

let check_vertical str is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  Char.code str.[3] - Char.code str.[1] = 0
  && go_down str true true is_sim r1 r2 r3 r4 r5 r6 r7 r8

let check_diagonal str is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  let col_dif = Char.code str.[3] - Char.code str.[1] in
  let row_diff = int_of_char str.[2] - int_of_char str.[0] in
  if abs row_diff = abs col_dif <> true then false
  else if (row_diff > 0 && col_dif > 0) || (row_diff < 0 && col_dif < 0)
  then
    go_diagonal "up_right" str true true is_sim r1 r2 r3 r4 r5 r6 r7 r8
  else if (row_diff > 0 && col_dif < 0) || (row_diff < 0 && col_dif > 0)
  then
    go_diagonal "up_left" str true true is_sim r1 r2 r3 r4 r5 r6 r7 r8
  else false

let rook_check input is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  check_vertical input is_sim r1 r2 r3 r4 r5 r6 r7 r8
  || check_horizontal input

let bishop_check input is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  check_diagonal input is_sim r1 r2 r3 r4 r5 r6 r7 r8

let queen_check input is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  check_diagonal input is_sim r1 r2 r3 r4 r5 r6 r7 r8
  || check_horizontal input
  || check_vertical input is_sim r1 r2 r3 r4 r5 r6 r7 r8

let king_check input is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  Char.code input.[3] - Char.code input.[1] |> abs = 1
  && int_of_char input.[2] - int_of_char input.[0] |> abs = 1
  && (check_horizontal input
     || check_vertical input is_sim r1 r2 r3 r4 r5 r6 r7 r8
     || check_diagonal input is_sim r1 r2 r3 r4 r5 r6 r7 r8)

let knight_check input =
  Char.code input.[3] - Char.code input.[1] |> abs = 2
  && int_of_char input.[2] - int_of_char input.[0] |> abs = 1
  || int_of_char input.[2] - int_of_char input.[0] |> abs = 2
     && Char.code input.[3] - Char.code input.[1] |> abs = 1

let pawn_check input moved color is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  let sign = if color = White then 1 else -1 in
  let row_diff = int_of_char input.[2] - int_of_char input.[0] in
  let col_diff = Char.code input.[3] - Char.code input.[1] in
  let int_rel_to_a = Char.code input.[3] - Char.code 'a' in
  let elt =
    if is_sim then
      Array.get (checkn3 input r1 r2 r3 r4 r5 r6 r7 r8) int_rel_to_a
    else Array.get (check3 input) int_rel_to_a
  in
  if row_diff = 1 * sign && abs col_diff <= 1 then
    match elt with
    | None ->
        check_vertical input is_sim r1 r2 r3 r4 r5 r6 r7 r8
        (* this means that the element is none -> can't go diagonally *)
    | Some _ -> check_diagonal input is_sim r1 r2 r3 r4 r5 r6 r7 r8
  else if (row_diff = 2 * sign && col_diff = 0) && moved = false then
    match elt with
    | None -> true
    | Some _ -> false
  else false

let castle i_p input o_p =
  match (i_p, o_p) with
  | Some i_p, Some o_p ->
      if get_level i_p = King && get_level o_p = Rook then
        if have_moved i_p || have_moved o_p then false
        else if input.[3] = 'h' || input.[3] = 'a' then
          check_horizontal input
        else false
      else if get_level i_p = Rook && get_level o_p = King then
        if have_moved i_p || have_moved o_p then false
        else if input.[1] = 'h' || input.[1] = 'a' then
          check_horizontal input
        else false
      else false
  | _, _ -> false

let check_piece ipc str opc is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  match ipc with
  | None -> false
  | Some pc ->
      (if get_level pc = Pawn then
       pawn_check str pc.moved (get_color pc) is_sim r1 r2 r3 r4 r5 r6
         r7 r8
      else if get_level pc = Rook then
        rook_check str is_sim r1 r2 r3 r4 r5 r6 r7 r8
      else if get_level pc = Bishop then
        bishop_check str is_sim r1 r2 r3 r4 r5 r6 r7 r8
      else if get_level pc = Knight then knight_check str
      else if get_level pc = Queen then
        queen_check str is_sim r1 r2 r3 r4 r5 r6 r7 r8
      else king_check str is_sim r1 r2 r3 r4 r5 r6 r7 r8)
      || castle ipc str opc

let color_checker i_p o_p input =
  match o_p with
  | None -> true
  | Some op -> (
      match i_p with
      | None -> false
      | Some ip ->
          if
            (castle i_p input o_p = false && get_color op = get_color ip)
            || castle i_p input o_p = true
               && get_color op <> get_color ip
          then false
          else true)

let promote_pawn input i_p =
  match i_p with
  | Some i_p ->
      if get_level i_p = Pawn then
        if input.[0] = '7' then
          if get_color i_p = White then
            if input.[2] = '8' then true else false
          else false
        else if input.[0] = '2' then
          if get_color i_p = Black then
            if input.[2] = '1' then true else false
          else false
        else false
      else false
  | None -> false

let rec loop_y x y p ppos r1 r2 r3 r4 r5 r6 r7 r8 =
  match y with
  | 8 -> []
  | curr_y ->
      let string_cmd =
        ppos ^ string_of_int x
        ^ (y + Char.code 'a' |> Char.chr |> Char.escaped)
      in
      let o_pr = check3 string_cmd in
      (*output piecerow*)
      let o_p =
        Char.code string_cmd.[3] - Char.code 'a' |> Array.get o_pr
      in
      (*output piece*)
      let new_y = curr_y + 1 in
      if
        check_piece (Some p) string_cmd o_p false r1 r2 r3 r4 r5 r6 r7
          r8
        && color_checker (Some p) o_p string_cmd
      then
        (p, string_cmd) :: loop_y x new_y p ppos r1 r2 r3 r4 r5 r6 r7 r8
      else loop_y x new_y p ppos r1 r2 r3 r4 r5 r6 r7 r8

let rec loop_x x y p ppos r1 r2 r3 r4 r5 r6 r7 r8 =
  match x with
  | 9 -> []
  | curr_x ->
      loop_y curr_x y p ppos r1 r2 r3 r4 r5 r6 r7 r8
      @ loop_x (curr_x + 1) y p ppos r1 r2 r3 r4 r5 r6 r7 r8

let rec has_move plist r1 r2 r3 r4 r5 r6 r7 r8 =
  match plist with
  | [] -> []
  | p :: t ->
      let ppos = string_of_pos p in
      loop_x 1 0 p ppos r1 r2 r3 r4 r5 r6 r7 r8
      @ has_move t r1 r2 r3 r4 r5 r6 r7 r8

(* given moves, try executing moves and seeing if king is still in
   check. if not, return false, else recurse*)
let rec has_legal_move plist king r1 r2 r3 r4 r5 r6 r7 r8 =
  match plist with
  | [] -> false
  | (p, cmd) :: t -> (
      let k = ref king in
      let a_bp = ref [] in
      let a_wp = ref [] in
      let arr1 = Array.copy r1 in
      let arr2 = Array.copy r2 in
      let arr3 = Array.copy r3 in
      let arr4 = Array.copy r4 in
      let arr5 = Array.copy r5 in
      let arr6 = Array.copy r6 in
      let arr7 = Array.copy r7 in
      let arr8 = Array.copy r8 in
      let o_r = checkn3 cmd arr1 arr2 arr3 arr4 arr5 arr6 arr7 arr8 in
      let moved_piece =
        Piece.place_piece
          (Some (cmd.[3], int_of_char cmd.[2] - int_of_char '0'))
          (Piece.get_color p) (Piece.get_level p) (Piece.get_rep p) true
      in
      if p = king then k := moved_piece;
      Some moved_piece
      |> Array.set o_r (Char.code cmd.[3] - Char.code 'a');
      Array.iter
        (fun y ->
          match y with
          | None -> ()
          | Some x ->
              if x.color = White then a_wp := x :: !a_wp
              else a_bp := x :: !a_bp)
        arr1;
      Array.iter
        (fun y ->
          match y with
          | None -> ()
          | Some x ->
              if x.color = White then a_wp := x :: !a_wp
              else a_bp := x :: !a_bp)
        arr2;
      Array.iter
        (fun y ->
          match y with
          | None -> ()
          | Some x ->
              if x.color = White then a_wp := x :: !a_wp
              else a_bp := x :: !a_bp)
        arr3;
      Array.iter
        (fun y ->
          match y with
          | None -> ()
          | Some x ->
              if x.color = White then a_wp := x :: !a_wp
              else a_bp := x :: !a_bp)
        arr4;
      Array.iter
        (fun y ->
          match y with
          | None -> ()
          | Some x ->
              if x.color = White then a_wp := x :: !a_wp
              else a_bp := x :: !a_bp)
        arr5;
      Array.iter
        (fun y ->
          match y with
          | None -> ()
          | Some x ->
              if x.color = White then a_wp := x :: !a_wp
              else a_bp := x :: !a_bp)
        arr6;
      Array.iter
        (fun y ->
          match y with
          | None -> ()
          | Some x ->
              if x.color = White then a_wp := x :: !a_wp
              else a_bp := x :: !a_bp)
        arr7;
      Array.iter
        (fun y ->
          match y with
          | None -> ()
          | Some x ->
              if x.color = White then a_wp := x :: !a_wp
              else a_bp := x :: !a_bp)
        arr8;
      match Piece.get_color king with
      | White ->
          if
            incheck !a_bp !k false arr1 arr2 arr3 arr4 arr5 arr6 arr7
              arr8
            <> true
          then true
          else has_legal_move t king r1 r2 r3 r4 r5 r6 r7 r8
      | Black ->
          (*simulating black moves to see if black has any legal moves -
            k = black king*)
          if
            incheck !a_wp !k false arr1 arr2 arr3 arr4 arr5 arr6 arr7
              arr8
            <> true
          then true
          else has_legal_move t king r1 r2 r3 r4 r5 r6 r7 r8)

and incheck plist king is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  match plist with
  | [] -> false
  | p :: t ->
      let cmd = king |> string_of_pos |> ( ^ ) (string_of_pos p) in
      let o_pr =
        if is_sim then checkn3 cmd r1 r2 r3 r4 r5 r6 r7 r8
        else check3 cmd
      in
      (*output piecerow*)
      let o_p = Char.code cmd.[3] - Char.code 'a' |> Array.get o_pr in
      (*output piece*)
      if check_piece (Some p) cmd o_p is_sim r1 r2 r3 r4 r5 r6 r7 r8
      then false
      else incheck t king is_sim r1 r2 r3 r4 r5 r6 r7 r8

let checkmated same_side_list opp_side_list king r1 r2 r3 r4 r5 r6 r7 r8
    =
  incheck same_side_list king true r1 r2 r3 r4 r5 r6 r7 r8
  (*using your pieces to check their king*)
  && has_legal_move
       (has_move opp_side_list r1 r2 r3 r4 r5 r6 r7 r8)
       (*their pieces have moves*)
       king r1 r2 r3 r4 r5 r6 r7 r8
     <> true
