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
      :: explode (String.sub st 1 (String.length str - 1))

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
      check_format (explode curr) ^ check_format (explode next)
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

let get_upper_col str =
  if Char.code str.[3] > Char.code str.[1] then
    Char.code str.[3] - Char.code 'a'
  else Char.code str.[1] - Char.code 'a'

let get_lower_col str =
  if Char.code str.[3] > Char.code str.[1] then
    Char.code str.[1] - Char.code 'a'
  else Char.code str.[3] - Char.code 'a'

let rec go_left lower_col upper_col row =
  if lower_col = upper_col - 1 || lower_col = upper_col then true
  else if Array.get row (upper_col - 1) <> None then false
  else go_left lower_col (upper_col - 1) row

let rec go_down str gate gate2 is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  if gate then
    if
      int_of_char str.[2] = int_of_char str.[0] - 1
      || int_of_char str.[2] = int_of_char str.[0] + 1
    then true
    else go_down str false true is_sim r1 r2 r3 r4 r5 r6 r7 r8
  else if int_of_char str.[2] = int_of_char str.[0] then true
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

let rec go_up_right str gate gate2 is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  if gate then
    if
      abs (int_of_char str.[2] - int_of_char str.[0]) = 1
      || abs (Char.code str.[3] - Char.code str.[1]) = 1
    then true
    else go_up_right str false true is_sim r1 r2 r3 r4 r5 r6 r7 r8
  else if
    int_of_char str.[2] = int_of_char str.[0]
    && Char.code str.[3] = Char.code str.[1]
  then true
  else if gate2 = true then
    if
      abs (int_of_char str.[2] - int_of_char str.[0]) = 1
      || abs (Char.code str.[3] - Char.code str.[1]) = 1
    then true
    else
      let new_str = str |> explode in
      match new_str with
      | [ row1; col1; row2; col2 ] ->
          if row1 < row2 then
            go_up_right
              (Char.escaped row1 ^ Char.escaped col1
              ^ string_of_int (Char.code row2 - Char.code '0' - 1)
              ^ Char.escaped (Char.chr (int_of_char col2 - 1)))
              false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
          else
            go_up_right
              (Char.escaped row1 ^ Char.escaped col1
              ^ string_of_int (Char.code row2 - Char.code '0' + 1)
              ^ Char.escaped (Char.chr (int_of_char col2 + 1)))
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
  else if
    abs (int_of_char str.[2] - int_of_char str.[0]) = 1
    || abs (Char.code str.[3] - Char.code str.[1]) = 1
  then true
  else
    let new_str = str |> explode in
    match new_str with
    | [ row1; col1; row2; col2 ] ->
        if row1 < row2 then
          go_up_right
            (Char.escaped row1 ^ Char.escaped col1
            ^ string_of_int (Char.code row2 - Char.code '0' - 1)
            ^ Char.escaped (Char.chr (int_of_char col2 - 1)))
            false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
        else
          go_up_right
            (Char.escaped row1 ^ Char.escaped col1
            ^ string_of_int (Char.code row2 - Char.code '0' + 1)
            ^ Char.escaped (Char.chr (int_of_char col2 + 1)))
            false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
    | _ -> false

let rec go_down_right str gate gate2 is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  if gate then
    if
      abs (int_of_char str.[2] - int_of_char str.[0]) = 1
      || abs (Char.code str.[3] - Char.code str.[1]) = 1
    then true
    else go_down_right str false true is_sim r1 r2 r3 r4 r5 r6 r7 r8
  else if
    int_of_char str.[2] = int_of_char str.[0]
    && Char.code str.[3] = Char.code str.[1]
  then true
  else if gate2 = true then
    if
      abs (int_of_char str.[2] - int_of_char str.[0]) = 1
      || abs (Char.code str.[3] - Char.code str.[1]) = 1
    then true
    else
      let new_str = str |> explode in
      match new_str with
      | [ row1; col1; row2; col2 ] ->
          if row2 < row1 then
            go_down_right
              (Char.escaped row1 ^ Char.escaped col1
              ^ string_of_int (Char.code row2 - Char.code '0' + 1)
              ^ Char.escaped (Char.chr (int_of_char col2 - 1)))
              false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
          else
            go_down_right
              (Char.escaped row1 ^ Char.escaped col1
              ^ string_of_int (Char.code row2 - Char.code '0' - 1)
              ^ Char.escaped (Char.chr (int_of_char col2 + 1)))
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
  else if
    abs (int_of_char str.[2] - int_of_char str.[0]) = 1
    || abs (Char.code str.[3] - Char.code str.[1]) = 1
  then true
  else
    let new_str = str |> explode in
    match new_str with
    | [ row1; col1; row2; col2 ] ->
        if row2 < row1 then
          go_down_right
            (Char.escaped row1 ^ Char.escaped col1
            ^ string_of_int (Char.code row2 - Char.code '0' + 1)
            ^ Char.escaped (Char.chr (int_of_char col2 - 1)))
            false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
        else
          go_down_right
            (Char.escaped row1 ^ Char.escaped col1
            ^ string_of_int (Char.code row2 - Char.code '0' - 1)
            ^ Char.escaped (Char.chr (int_of_char col2 + 1)))
            false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
    | _ -> false

let rec go_up_left str gate gate2 is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  if gate then
    if
      abs (int_of_char str.[2] - int_of_char str.[0]) = 1
      || abs (Char.code str.[3] - Char.code str.[1]) = 1
    then true
    else go_up_left str false true is_sim r1 r2 r3 r4 r5 r6 r7 r8
  else if
    int_of_char str.[2] = int_of_char str.[0]
    && Char.code str.[3] = Char.code str.[1]
  then true
  else if gate2 then
    if
      abs (int_of_char str.[2] - int_of_char str.[0]) = 1
      || abs (Char.code str.[3] - Char.code str.[1]) = 1
    then true
    else
      let new_str = str |> explode in
      match new_str with
      | [ row1; col1; row2; col2 ] ->
          if row1 < row2 then
            go_up_left
              (Char.escaped row1 ^ Char.escaped col1
              ^ string_of_int (Char.code row2 - Char.code '0' - 1)
              ^ Char.escaped (Char.chr (int_of_char col2 + 1)))
              false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
          else
            go_up_left
              (Char.escaped row1 ^ Char.escaped col1
              ^ string_of_int (Char.code row2 - Char.code '0' + 1)
              ^ Char.escaped (Char.chr (int_of_char col2 - 1)))
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
  else if
    abs (int_of_char str.[2] - int_of_char str.[0]) = 1
    || abs (Char.code str.[3] - Char.code str.[1]) = 1
  then true
  else
    let new_str = str |> explode in
    match new_str with
    | [ row1; col1; row2; col2 ] ->
        if row1 < row2 then
          go_up_left
            (Char.escaped row1 ^ Char.escaped col1
            ^ string_of_int (Char.code row2 - Char.code '0' - 1)
            ^ Char.escaped (Char.chr (int_of_char col2 + 1)))
            false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
        else
          go_up_left
            (Char.escaped row1 ^ Char.escaped col1
            ^ string_of_int (Char.code row2 - Char.code '0' + 1)
            ^ Char.escaped (Char.chr (int_of_char col2 - 1)))
            false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
    | _ -> false

let rec go_down_left str gate gate2 is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  if gate then
    if
      abs (int_of_char str.[2] - int_of_char str.[0]) = 1
      || abs (Char.code str.[3] - Char.code str.[1]) = 1
    then true
    else go_down_left str false true is_sim r1 r2 r3 r4 r5 r6 r7 r8
  else if
    int_of_char str.[2] = int_of_char str.[0]
    && Char.code str.[3] = Char.code str.[1]
  then true
  else if gate2 then
    if
      abs (int_of_char str.[2] - int_of_char str.[0]) = 1
      || abs (Char.code str.[3] - Char.code str.[1]) = 1
    then true
    else
      let new_str = str |> explode in
      match new_str with
      | [ row1; col1; row2; col2 ] ->
          if row2 < row1 then
            go_down_left
              (Char.escaped row1 ^ Char.escaped col1
              ^ string_of_int (Char.code row2 - Char.code '0' + 1)
              ^ Char.escaped (Char.chr (int_of_char col2 + 1)))
              false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
          else
            go_down_left
              (Char.escaped row1 ^ Char.escaped col1
              ^ string_of_int (Char.code row2 - Char.code '0' - 1)
              ^ Char.escaped (Char.chr (int_of_char col2 - 1)))
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
  else if
    abs (int_of_char str.[2] - int_of_char str.[0]) = 1
    || abs (Char.code str.[3] - Char.code str.[1]) = 1
  then true
  else
    let new_str = str |> explode in
    match new_str with
    | [ row1; col1; row2; col2 ] ->
        if row2 < row1 then
          go_down_left
            (Char.escaped row1 ^ Char.escaped col1
            ^ string_of_int (Char.code row2 - Char.code '0' + 1)
            ^ Char.escaped (Char.chr (int_of_char col2 + 1)))
            false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
        else
          go_down_left
            (Char.escaped row1 ^ Char.escaped col1
            ^ string_of_int (Char.code row2 - Char.code '0' - 1)
            ^ Char.escaped (Char.chr (int_of_char col2 - 1)))
            false false is_sim r1 r2 r3 r4 r5 r6 r7 r8
    | _ -> false

let check_horizontal str =
  int_of_char str.[2] - int_of_char str.[0] = 0
  && go_left (get_lower_col str) (get_upper_col str) (check1 str)

let check_vertical str is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  Char.code str.[3] - Char.code str.[1] = 0
  && (go_down str true) true is_sim r1 r2 r3 r4 r5 r6 r7 r8

let check_diagonal str is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  print_endline "check diagonal";
  if
    abs (int_of_char str.[2] - int_of_char str.[0])
    = abs (Char.code str.[3] - Char.code str.[1])
    <> true
  then false
  else if
    int_of_char str.[2] > int_of_char str.[0]
    && Char.code str.[3] > Char.code str.[1]
  then (go_up_right str true) true is_sim r1 r2 r3 r4 r5 r6 r7 r8
  else if
    int_of_char str.[2] > int_of_char str.[0]
    && Char.code str.[3] < Char.code str.[1]
  then
    (print_endline "goingupleft";
     (go_up_left str true) true)
      is_sim r1 r2 r3 r4 r5 r6 r7 r8
  else if
    int_of_char str.[2] < int_of_char str.[0]
    && Char.code str.[3] > Char.code str.[1]
  then (go_down_right str true) true is_sim r1 r2 r3 r4 r5 r6 r7 r8
  else (go_down_left str true) true is_sim r1 r2 r3 r4 r5 r6 r7 r8

let rook_check input is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  check_vertical input is_sim r1 r2 r3 r4 r5 r6 r7 r8
  || check_horizontal input

let bishop_check input is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  check_diagonal input is_sim r1 r2 r3 r4 r5 r6 r7 r8

let queen_check input is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  check_diagonal input is_sim r1 r2 r3 r4 r5 r6 r7 r8
  || check_horizontal input
  || check_vertical input is_sim r1 r2 r3 r4 r5 r6 r7 r8

(* let king_check input is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
   check_horizontal input && abs (Char.code input.[3] - Char.code
   input.[1]) = 1 || check_vertical input is_sim r1 r2 r3 r4 r5 r6 r7 r8
   && abs (int_of_char input.[2] - int_of_char input.[1]) = 1 ||
   check_diagonal input is_sim r1 r2 r3 r4 r5 r6 r7 r8 && abs (Char.code
   input.[3] - Char.code input.[1]) = 1 || abs (int_of_char input.[2] -
   int_of_char input.[0]) = 1 *)

let king_check input is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  abs (Char.code input.[3] - Char.code input.[1]) = 1
  && abs (int_of_char input.[2] - int_of_char input.[0]) = 1
  && (check_horizontal input
     || check_vertical input is_sim r1 r2 r3 r4 r5 r6 r7 r8
     || check_diagonal input is_sim r1 r2 r3 r4 r5 r6 r7 r8)

let knight_check input =
  abs (Char.code input.[3] - Char.code input.[1]) = 2
  && abs (int_of_char input.[2] - int_of_char input.[0]) = 1
  || abs (int_of_char input.[2] - int_of_char input.[0]) = 2
     && abs (Char.code input.[3] - Char.code input.[1]) = 1

let pawn_check input moved color is_sim r1 r2 r3 r4 r5 r6 r7 r8 =
  let sign = if color = White then 1 else -1 in
  if
    int_of_char input.[2] - int_of_char input.[0] = 1 * sign
    && abs (Char.code input.[3] - Char.code input.[1]) <= 1
  then
    let get_element =
      Array.get (check3 input) (Char.code input.[3] - 97)
    in
    match get_element with
    | None ->
        check_vertical input is_sim r1 r2 r3 r4 r5 r6 r7 r8
        (* this means that the element is none -> can't go diagonally *)
    | Some _ -> check_diagonal input is_sim r1 r2 r3 r4 r5 r6 r7 r8
  else if
    (int_of_char input.[2] - int_of_char input.[0] = 2 * sign
    && Char.code input.[3] = Char.code input.[1])
    && moved = false
  then
    let get_elt =
      if is_sim then
        Array.get
          (checkn3 input r1 r2 r3 r4 r5 r6 r7 r8)
          (Char.code input.[3] - 97)
      else Array.get (check3 input) (Char.code input.[3] - 97)
    in
    match get_elt with
    | None -> true
    | Some _ -> false
  else false

let castle i_p input o_p =
  match (i_p, o_p) with
  | Some i_p, Some o_p ->
      if get_level i_p = King then
        if get_level o_p = Rook then
          if have_moved i_p || have_moved o_p then false
          else if input.[3] = 'h' then check_horizontal input
          else if input.[3] = 'a' then check_horizontal input
          else false
        else false
      else if get_level i_p = Rook then
        if get_level o_p = King then
          if have_moved i_p || have_moved o_p then false
          else if input.[1] = 'h' then check_horizontal input
          else if input.[1] = 'a' then check_horizontal input
          else false
        else false
      else false
  | None, _ -> false
  | _, None -> false

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
      else if get_level pc = Queen then (
        print_endline "queen";
        queen_check str is_sim r1 r2 r3 r4 r5 r6 r7 r8)
      else king_check str is_sim r1 r2 r3 r4 r5 r6 r7 r8)
      || castle ipc str opc

let color_checker i_p o_p input =
  match o_p with
  | None -> true
  | Some op -> (
      match i_p with
      | None -> false
      | Some ip ->
          if castle i_p input o_p = false && get_color op = get_color ip
          then false
          else if
            castle i_p input o_p = true && get_color op <> get_color ip
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

let rec loopy x y p ppos r1 r2 r3 r4 r5 r6 r7 r8 =
  match y with
  | 8 -> []
  | curry ->
      let string_cmd =
        ppos ^ string_of_int x
        ^ Char.escaped (Char.chr (y + Char.code 'a'))
      in
      let o_pr = check3 string_cmd in
      (*output piecerow*)
      let o_p = Array.get o_pr (Char.code string_cmd.[3] - 97) in
      (*output piece*)
      if
        check_piece (Some p) string_cmd o_p false r1 r2 r3 r4 r5 r6 r7
          r8
        && color_checker (Some p) o_p string_cmd
      then (
        print_endline "loopy";
        print_endline p.rep;
        let chr, num =
          match p.position with
          | None -> failwith "p no pos"
          | Some (chr1, num1) -> (chr1, num1)
        in
        print_endline (Char.escaped chr);
        print_endline (string_of_int num);
        (match o_p with
        | None -> print_endline ""
        | Some pe ->
            print_endline pe.rep;
            print_endline string_cmd);
        (p, string_cmd)
        :: loopy x (curry + 1) p ppos r1 r2 r3 r4 r5 r6 r7 r8)
      else loopy x (curry + 1) p ppos r1 r2 r3 r4 r5 r6 r7 r8

let rec loopx x y p ppos r1 r2 r3 r4 r5 r6 r7 r8 =
  match x with
  | 9 -> []
  | currx ->
      loopy currx y p ppos r1 r2 r3 r4 r5 r6 r7 r8
      @ loopx (currx + 1) y p ppos r1 r2 r3 r4 r5 r6 r7 r8

let rec has_move plist r1 r2 r3 r4 r5 r6 r7 r8 =
  match plist with
  | [] -> []
  | p :: t ->
      let ppos =
        match p.position with
        | None -> failwith "p has no position"
        | Some (col, row) ->
            string_of_int row ^ Char.escaped (Char.chr (Char.code col))
      in
      loopx 1 0 p ppos r1 r2 r3 r4 r5 r6 r7 r8
      @ has_move t r1 r2 r3 r4 r5 r6 r7 r8

(*This is the row that corresponds to the output space that the user
  selects*)
let rec print_lst lst =
  match lst with
  | [] -> ""
  | (p, cmd) :: t -> p.rep ^ cmd ^ print_lst t

let rec print_p lst =
  match lst with
  | [] -> ""
  | p :: t -> p.rep ^ print_p t

(* given moves, try executing moves and seeing if king is still in
   check. if not, return false, else recurse*)
let rec has_legal_move plist king r1 r2 r3 r4 r5 r6 r7 r8 =
  print_endline (print_lst plist);
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
      Array.set o_r (Char.code cmd.[3] - 97) (Some moved_piece);
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
          print_endline "a_bp";
          print_endline (print_p !a_bp);
          if
            incheck !a_bp !k false arr1 arr2 arr3 arr4 arr5 arr6 arr7
              arr8
            <> true
          then (
            print_endline "not in check";
            true)
          else has_legal_move t king r1 r2 r3 r4 r5 r6 r7 r8
      | Black ->
          (*simulating black moves to see if black has any legal moves -
            k = black king*)
          if
            incheck !a_wp !k false arr1 arr2 arr3 arr4 arr5 arr6 arr7
              arr8
            <> true
          then (
            print_endline "not in check";
            true)
          else has_legal_move t king r1 r2 r3 r4 r5 r6 r7 r8)

and incheck plist king is_real r1 r2 r3 r4 r5 r6 r7 r8 =
  match plist with
  | [] -> false
  | p :: t ->
      let ppos =
        match p.position with
        | None -> failwith "p has no position"
        | Some (col, row) ->
            string_of_int row ^ Char.escaped (Char.chr (Char.code col))
      in
      let kpos =
        match king.position with
        | None -> failwith "p has no position"
        | Some (col, row) ->
            string_of_int row ^ Char.escaped (Char.chr (Char.code col))
      in
      let o_pr =
        if is_real then check3 (ppos ^ kpos)
        else (
          print_endline "checkn3";
          checkn3 (ppos ^ kpos) r1 r2 r3 r4 r5 r6 r7 r8)
      in
      (*output piecerow*)
      let o_p = Array.get o_pr (Char.code (ppos ^ kpos).[3] - 97) in
      (*output piece*)
      print_endline "p";
      print_endline p.rep;
      print_endline "cmd";
      print_endline (ppos ^ kpos);
      let check_check = if is_real then false else true in
      if
        check_piece (Some p) (ppos ^ kpos) o_p check_check r1 r2 r3 r4
          r5 r6 r7 r8
      then true
      else incheck t king is_real r1 r2 r3 r4 r5 r6 r7 r8

let checkmated same_side_list opp_side_list king r1 r2 r3 r4 r5 r6 r7 r8
    =
  incheck same_side_list king true r1 r2 r3 r4 r5 r6 r7 r8
  (*using your pieces to check their king*)
  && has_legal_move
       (has_move opp_side_list r1 r2 r3 r4 r5 r6 r7 r8)
       (*their pieces have moves*)
       king r1 r2 r3 r4 r5 r6 r7 r8
     <> true
