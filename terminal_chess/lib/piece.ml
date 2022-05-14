type level =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

type color =
  | White
  | Black

type position = (char * int) option

exception NoPiece

exception NoPosition

type piece = {
  position : position;
  color : color;
  level : level;
  rep : string;
  moved : bool;
}

let rep space =
  match space with
  | None -> raise NoPiece
  | Some space -> space.rep

let get_level p = p.level

let get_color p = p.color

let get_position p = p.position

let string_of_pos p =
  match p.position with
  | None -> raise NoPosition
  | Some (col, row) ->
      string_of_int row ^ Char.escaped (Char.chr (Char.code col))

let get_rep p = p.rep

let place_piece pos c l rep move =
  { position = pos; color = c; level = l; rep; moved = move }

let have_moved p = p.moved
