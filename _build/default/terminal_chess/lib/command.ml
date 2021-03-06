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
   move from concating 2 2 letter strings. Ex: (2,b) (3,b) -> 2b3b *)
let check_valid_move str =
  match str with
  | [] -> raise EmptyCommand
  | [ _; curr; next ] ->
      explode next |> check_format
      |> ( ^ ) (explode curr |> check_format)
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
let check1 (str : string) grid =
  let row = int_of_char str.[0] - int_of_char '0' - 1 in
  if row < 0 || row > 8 then raise InvalidInput else Array.get grid row

(*This is the row that corresponds to the output space that the user
  selects*)
let check3 (str : string) grid =
  let row = int_of_char str.[2] - int_of_char '0' - 1 in
  if row < 0 || row > 8 then raise InvalidInput else Array.get grid row

let rec go_left lower_col upper_col row =
  if lower_col = upper_col - 1 || lower_col = upper_col then true
  else if Array.get row (upper_col - 1) <> None then false
  else go_left lower_col (upper_col - 1) row

let exists_in_between str grid gate =
  gate = false
  && Char.code str.[3] - Char.code 'a'
     |> Array.get (check3 str grid)
     <> None

let rec go_down str gate grid =
  let row_diff = int_of_char str.[2] - int_of_char str.[0] in
  if exists_in_between str grid gate then false
  else if row_diff |> abs = 1 then true
  else if row_diff = 0 then false
  else
    let new_str = str |> explode in
    match new_str with
    | [ row1; col1; row2; col2 ] ->
        let prev_loc = Char.escaped row1 ^ Char.escaped col1 in
        let dir = if row2 < row1 then 1 else -1 in
        let new_o_row = Char.code row2 - Char.code '0' + dir in
        go_down
          (prev_loc ^ string_of_int new_o_row ^ Char.escaped col2)
          false grid
    | _ -> false

let rec go_diagonal direction str gate grid =
  let diagonal_direction = if direction = "up_right" then 1 else -1 in
  let row_diff = int_of_char str.[2] - int_of_char str.[0] in
  let col_diff = Char.code str.[3] - Char.code str.[1] in
  if exists_in_between str grid gate then false
  else if abs row_diff = 1 && abs col_diff = 1 then true
  else if row_diff = 0 && col_diff = 0 then false
  else
    let new_str = str |> explode in
    match new_str with
    | [ row1; col1; row2; col2 ] ->
        let prev_loc = Char.escaped row1 ^ Char.escaped col1 in
        let hor_dir = if row2 < row1 then 1 else -1 in
        let new_o_row = Char.code row2 - Char.code '0' + hor_dir in
        let new_o_col =
          int_of_char col2 + (diagonal_direction * hor_dir)
          |> Char.chr |> Char.escaped
        in
        go_diagonal direction
          (prev_loc ^ (new_o_row |> string_of_int) ^ new_o_col)
          false grid
    | _ -> false

let check_horizontal str grid =
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
  && go_left get_lower_col get_upper_col (check1 str grid)

let check_vertical str grid =
  Char.code str.[3] - Char.code str.[1] = 0 && go_down str true grid

let check_diagonal str grid =
  let col_dif = Char.code str.[3] - Char.code str.[1] in
  let row_diff = int_of_char str.[2] - int_of_char str.[0] in
  if abs row_diff = abs col_dif <> true then false
  else if (row_diff > 0 && col_dif > 0) || (row_diff < 0 && col_dif < 0)
  then go_diagonal "up_right" str true grid
  else if (row_diff > 0 && col_dif < 0) || (row_diff < 0 && col_dif > 0)
  then go_diagonal "up_left" str true grid
  else false

let rook_check input grid =
  check_vertical input grid || check_horizontal input grid

let bishop_check input grid = check_diagonal input grid

let queen_check input grid =
  check_diagonal input grid
  || check_horizontal input grid
  || check_vertical input grid

let king_check input grid =
  (Char.code input.[3] - Char.code input.[1] |> abs = 1
   && int_of_char input.[2] - int_of_char input.[0] |> abs = 1
  || Char.code input.[3] - Char.code input.[1] |> abs = 1
     && int_of_char input.[2] - int_of_char input.[0] |> abs = 0
  || Char.code input.[3] - Char.code input.[1] |> abs = 0
     && int_of_char input.[2] - int_of_char input.[0] |> abs = 1)
  && (check_horizontal input grid
     || check_vertical input grid
     || check_diagonal input grid)

let knight_check input =
  Char.code input.[3] - Char.code input.[1] |> abs = 2
  && int_of_char input.[2] - int_of_char input.[0] |> abs = 1
  || int_of_char input.[2] - int_of_char input.[0] |> abs = 2
     && Char.code input.[3] - Char.code input.[1] |> abs = 1

let pawn_check input moved color grid =
  let sign = if color = White then 1 else -1 in
  let row_diff = int_of_char input.[2] - int_of_char input.[0] in
  let col_diff = Char.code input.[3] - Char.code input.[1] in
  let int_rel_to_a = Char.code input.[3] - Char.code 'a' in
  let elt = Array.get (check3 input grid) int_rel_to_a in
  if row_diff = 1 * sign && abs col_diff <= 1 then
    match elt with
    | None ->
        check_vertical input grid
        (* this means that the element is none -> can't go diagonally *)
    | Some _ -> check_diagonal input grid
  else if (row_diff = 2 * sign && col_diff = 0) && moved = false then
    match elt with
    | None -> true
    | Some _ -> false
  else false

let castle i_p input o_p grid =
  match (i_p, o_p) with
  | Some i_p, Some o_p ->
      if have_moved i_p || have_moved o_p then ("na", false)
      else if check_horizontal input grid = false then ("na", false)
      else if
        (get_level i_p = King && get_level o_p = Rook)
        || (get_level i_p = King && get_level o_p = Rook)
      then
        if input.[3] = 'h' || input.[1] = 'h' then ("ksik", true)
        else if input.[3] = 'a' || input.[1] = 'a' then ("qsik", true)
        else ("na", false)
      else ("na", false)
  | _, _ -> ("na", false)

let check_piece ipc str opc grid =
  match ipc with
  | None -> false
  | Some pc ->
      (if get_level pc = Pawn then
       pawn_check str pc.moved (get_color pc) grid
      else if get_level pc = Rook then rook_check str grid
      else if get_level pc = Bishop then bishop_check str grid
      else if get_level pc = Knight then knight_check str
      else if get_level pc = Queen then queen_check str grid
      else king_check str grid)
      || snd (castle ipc str opc grid)

let color_checker i_p o_p input grid =
  match o_p with
  | None -> true
  | Some op -> (
      match i_p with
      | None -> false
      | Some ip ->
          if
            snd (castle i_p input o_p grid) = false
            && get_color op = get_color ip
            || snd (castle i_p input o_p grid) = true
               && get_color op <> get_color ip
          then false
          else true)

(**If a white pawn moves from row 7 to row 8, or a black pawn moves from
   row 1 to row 2, then set it up for promotion.*)
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

let rec loop_y x y p ppos grid =
  match y with
  | 8 -> []
  | curr_y ->
      let string_cmd =
        ppos ^ string_of_int x
        ^ (y + Char.code 'a' |> Char.chr |> Char.escaped)
      in
      let o_pr = check3 string_cmd grid in
      (*output piecerow*)
      let o_p =
        Char.code string_cmd.[3] - Char.code 'a' |> Array.get o_pr
      in
      (*output piece*)
      let new_y = curr_y + 1 in
      if
        check_piece (Some p) string_cmd o_p grid
        && color_checker (Some p) o_p string_cmd grid
      then (p, string_cmd) :: loop_y x new_y p ppos grid
      else loop_y x new_y p ppos grid

let rec loop_x x y p ppos grid =
  match x with
  | 9 -> []
  | curr_x ->
      loop_y curr_x y p ppos grid @ loop_x (curr_x + 1) y p ppos grid

let rec has_move w_plist b_plist king grid =
  let color = get_color king in
  let plist = if color = White then w_plist else b_plist in
  match plist with
  | [] -> []
  | p :: t ->
      let new_lst =
        if color = White then has_move t b_plist king grid
        else has_move w_plist t king grid
      in
      let ppos = string_of_pos p in
      loop_x 1 0 p ppos grid @ new_lst

let update_avail_lst a_wp a_bp grid =
  for x = 0 to 7 do
    Array.iter
      (fun y ->
        match y with
        | None -> ()
        | Some x ->
            if get_color x = White then a_wp := x :: !a_wp
            else a_bp := x :: !a_bp)
      (Array.get grid x)
  done

(* given moves, try executing moves and seeing if king is still in
   check. if not, return false, else recurse*)
let rec has_legal_move plist king grid =
  match plist with
  | [] -> false
  | (p, cmd) :: t ->
      let k = ref king in
      let a_bp = ref [] in
      let a_wp = ref [] in
      let copy_grid = Array.map Array.copy grid in
      let i_r = check1 cmd copy_grid in
      let i_col = Char.code cmd.[1] - Char.code 'a' in
      let o_r = check3 cmd copy_grid in
      let o_col = Char.code cmd.[3] - Char.code 'a' in
      let new_pos =
        {
          row = int_of_char cmd.[2] - int_of_char '0';
          col = Char.escaped cmd.[3];
        }
      in
      let moved_piece = { p with position = new_pos; moved = true } in
      if p = king then k := moved_piece;
      Some moved_piece |> Array.set o_r o_col;
      None |> Array.set i_r i_col;
      update_avail_lst a_wp a_bp copy_grid;
      if incheck !a_wp !a_bp !k copy_grid <> true then true
      else has_legal_move t king grid

and incheck w_plist b_plist king grid =
  let color = get_color king in
  let plist = if color = White then b_plist else w_plist in
  match plist with
  | [] -> false
  | p :: t ->
      let cmd = king |> string_of_pos |> ( ^ ) (string_of_pos p) in
      let o_pr = check3 cmd grid in
      (*output piecerow*)
      let o_p = Char.code cmd.[3] - Char.code 'a' |> Array.get o_pr in
      (*output piece*)
      if check_piece (Some p) cmd o_p grid then true
      else if color = White then incheck w_plist t king grid
      else incheck t b_plist king grid

let checkmated w_p_list b_p_list king grid =
  incheck w_p_list b_p_list king grid
  && has_legal_move (has_move w_p_list b_p_list king grid) king grid
     <> true

let rec print_list = function
  | [] -> ()
  | e :: l ->
      print_string e;
      print_string " ";
      print_list l

let color_matcher board space =
  match space with
  | None ->
      print_endline "calling None";
      raise NoPiece
  | Some space -> board.model.turn <> get_color space

let rules () =
  print_endline
    "Rules of Chess (taken from chessusa.com):\n\n\
    \  Pieces:\n\
    \  \n\
    \  The King is the most important piece on the chessboard. If he \
     is checkmated (see objectives, below) the game is over! The King \
     in chess can move one space in any direction. He can never move \
     into 'check' (where he is threatened by another piece). This \
     means the king can never be in the space adjacent to the opposing \
     King. The King in chess can also castle. 'Castling' is described \
     in the special moves section below.\n\
    \  \n\
    \  The Queen is often considered the most powerful piece on the \
     chessboard. She is placed next to the king. The game is not over \
     when she is lost, but if your opponent has a Queen and you do \
     not, you may find yourself at a considerable disadvantage! Like \
     the King, the Queen can move in any direction. However, she is \
     not limited to just one space - she can move any number of spaces \
     in any direction, as long as she is not obstructed by another \
     piece. Of course, if that obstruction is an opposing piece, she \
     is free to capture it!\n\
    \  \n\
    \  There are two Bishops for each player on the chessboard, \
     situated next to Queen and the King, respectively. These pieces \
     move along the diagonals of the chessboard. You can see that this \
     means that the Bishop is bound to the color square it starts on. \
     One Bishop starts on a white square, and one starts on a black \
     square. They can move any number of spaces on the diagonals as \
     long as they are not obstructed by another piece, Of course, if \
     that obstruction is an opposing piece, they are free to capture \
     it!\n\
    \  \n\
    \  There are two Knights for each player on the chessboard, \
     situated between the Bishop and the Rook. The Knight has the \
     unique trait of being able to 'leap' over other pieces. They move \
     in an 'L' shape. If they land on an opposing piece, it is \
     captured.\n\
    \  \n\
    \  There are two Rooks for each player on the chessboard, situated \
     on the corners, next to the Knight. These pieces move up and down \
     the rank and file of the chessboard, and can move any number of \
     spaces as long as they are not obstructed by another piece. If \
     the obstructing piece belong to their opponent, they are free to \
     capture it. The Rook can also castle with the King. 'Castling' is \
     described in the special moves section below.\n\
    \  \n\
    \  Each player has eight Pawns. There are several unique \
     attributes with regards to Pawn moves in chess. The 'Pawn First' \
     move rules state each pawn has the option to move forward one \
     space or two spaces. After this move, they can only move one \
     space forward. However, they are also the only piece that \
     captures in a method different from how they move. To capture, \
     the Pawn moves diagonally one space. The Pawn can never move \
     backwards. But what happens when a pawn reaches the other side? \
     If the Pawn reaches the opposite side of the chessboard, it has \
     the unique ability to promote to another piece. The pawn can \
     become a Queen, Bishop, Rook, or Knight. There are no \
     restrictions to how many pieces of a given type you can have via \
     promotion.\n\
    \  \n\
    \  Objectives:\n\
    \  \n\
    \  The objective in chess is to checkmate your opponents King, and \
     there are three potential ways the game can end:\n\
    \  \n\
    \  First, you can checkmate your opponent. This means that the \
     King is in check (under potential attack from an opposing piece) \
     and the player can not make any legal move to remove the King \
     from check. At this point, the game is over and the checkmated \
     player loses. The amount of material on the chessboard is of no \
     concern.\n\
    \  \n\
    \  Second, you and your opponent can reach a stalemate - the \
     opponents King is not currently in check, but would be force to \
     move in to check with their next move. Because you can never put \
     your own King in check, you would have no legal moves to make. A \
     stalemate does not mean the attacking player has won. Instead, it \
     is a draw - neither player is victorious.\n\
    \  \n\
    \  Special Moves:\n\
    \  \n\
    \  Castling, otherwise known as the rook and king switch, is a \
     move that involves the King and the Rook. This is the only \
     situation in which you would move two of your own pieces in the \
     same move. The King and the Rook move towards each other and swap \
     places. To do this, move your King not one, but two spaces \
     towards the Rook you are castling with. Then place the Rook on \
     the opposite side of the King. This can be done on either the \
     King side or Queen side, however there are several prerequisites:\n\
    \    1. The king and rook must not have moved thus far\n\
    \    2. There must not be any obstructing pieces between them\n\
    \    3. Remember, the king cannot castle into check (once castling \
     is completed, the king cannot end up in check)."
