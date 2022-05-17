open Yojson.Basic.Util

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

type t = { pieces : piece list }

(** to go in board.ml: let piecesjson = Yojson.Basic.from_file
    "pieces.json" let board = from_json piecesjson *)

let from_json j =
  { pieces = j |> member "pieces" |> to_list |> List.map piece_of_json }

let position_of_json j =
  {
    row = j |> member "row" |> to_string;
    col = j |> member "col" |> to_int;
  }

let piece_of_json j =
  {
    name = j |> member "name" |> to_string;
    position = j |> member "position" |> List.map position_of_json;
    color = j |> member "color" |> to_string;
    level = j |> member "level" |> to_string;
    rep = j |> member "rep" |> to_string;
    moved = j |> member "moved" |> to_bool;
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
