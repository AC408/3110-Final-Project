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

type position = {
  row : int;
  col : string;
}

exception NoPiece
exception NoPosition

type piece = {
  name : string;
  position : position;
  color : string;
  level : string;
  rep : string;
  moved : bool;
}

type t = { pieces : piece list }

let pieces t = t.pieces

(** to go in board.ml: let piecesjson = Yojson.Basic.from_file
    "pieces.json" let board = from_json piecesjson *)

let position_of_json j =
  {
    row = j |> member "row" |> to_int;
    col = j |> member "col" |> to_string;
  }

let piece_of_json j =
  {
    name = j |> member "name" |> to_string;
    position = j |> member "position" |> position_of_json;
    color = j |> member "color" |> to_string;
    level = j |> member "level" |> to_string;
    rep = j |> member "rep" |> to_string;
    moved = j |> member "moved" |> to_bool;
  }

let t_from_json j =
  { pieces = j |> member "pieces" |> to_list |> List.map piece_of_json }

let parsejson j =
  try t_from_json j with
  | Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let from_json json = parsejson json

let rep space =
  match space with
  | None -> raise NoPiece
  | Some space -> space.rep

let get_level p =
  match p.level with
  | "Pawn" -> Pawn
  | "Knight" -> Knight
  | "Rook" -> Rook
  | "Bishop" -> Bishop
  | "Queen" -> Queen
  | "King" -> King
  | _ -> failwith "this is not a valid level"

let get_color p =
  match p.color with
  | "White" -> White
  | "Black" -> Black
  | _ -> failwith "this is not a valid color"

let get_name p = p.name
let get_position p = p.position
let string_of_pos p = string_of_int p.position.row ^ p.position.col
let get_rep p = p.rep

let place_piece name pos c l rep move =
  { name; position = pos; color = c; level = l; rep; moved = move }

let have_moved p = p.moved
