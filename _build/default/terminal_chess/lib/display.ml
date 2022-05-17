open Piece
open Board

type piecerow = piece option array

type board = {
  grid : piecerow array;
  graveyard : string list;
  model : model;
}

let jsonfile =
  "data" ^ Filename.dir_sep ^ "pieces.json"
  |> Yojson.Basic.from_file |> Piece.t_from_json

let blackrook1 =
  List.find
    (fun x -> if Piece.get_name x = "blackrook1" then true else false)
    (Piece.pieces jsonfile)

let blackknight1 =
  List.find
    (fun x -> if Piece.get_name x = "blackknight1" then true else false)
    (Piece.pieces jsonfile)

let blackbishop1 =
  List.find
    (fun x -> if Piece.get_name x = "blackbishop1" then true else false)
    (Piece.pieces jsonfile)

let blackqueen =
  List.find
    (fun x -> if Piece.get_name x = "blackqueen" then true else false)
    (Piece.pieces jsonfile)

let blackking =
  List.find
    (fun x -> if Piece.get_name x = "blackking" then true else false)
    (Piece.pieces jsonfile)

let blackbishop2 =
  List.find
    (fun x -> if Piece.get_name x = "blackbishop2" then true else false)
    (Piece.pieces jsonfile)

let blackknight2 =
  List.find
    (fun x -> if Piece.get_name x = "blackknight2" then true else false)
    (Piece.pieces jsonfile)

let blackrook2 =
  List.find
    (fun x -> if Piece.get_name x = "blackrook2" then true else false)
    (Piece.pieces jsonfile)

let blackpawn1 =
  List.find
    (fun x -> if Piece.get_name x = "blackpawn1" then true else false)
    (Piece.pieces jsonfile)

let blackpawn2 =
  List.find
    (fun x -> if Piece.get_name x = "blackpawn2" then true else false)
    (Piece.pieces jsonfile)

let blackpawn3 =
  List.find
    (fun x -> if Piece.get_name x = "blackpawn3" then true else false)
    (Piece.pieces jsonfile)

let blackpawn4 =
  List.find
    (fun x -> if Piece.get_name x = "blackpawn4" then true else false)
    (Piece.pieces jsonfile)

let blackpawn5 =
  List.find
    (fun x -> if Piece.get_name x = "blackpawn5" then true else false)
    (Piece.pieces jsonfile)

let blackpawn6 =
  List.find
    (fun x -> if Piece.get_name x = "blackpawn6" then true else false)
    (Piece.pieces jsonfile)

let blackpawn7 =
  List.find
    (fun x -> if Piece.get_name x = "blackpawn7" then true else false)
    (Piece.pieces jsonfile)

let blackpawn8 =
  List.find
    (fun x -> if Piece.get_name x = "blackpawn8" then true else false)
    (Piece.pieces jsonfile)

let whitepawn1 =
  List.find
    (fun x -> if Piece.get_name x = "whitepawn1" then true else false)
    (Piece.pieces jsonfile)

let whitepawn2 =
  List.find
    (fun x -> if Piece.get_name x = "whitepawn2" then true else false)
    (Piece.pieces jsonfile)

let whitepawn3 =
  List.find
    (fun x -> if Piece.get_name x = "whitepawn3" then true else false)
    (Piece.pieces jsonfile)

let whitepawn4 =
  List.find
    (fun x -> if Piece.get_name x = "whitepawn4" then true else false)
    (Piece.pieces jsonfile)

let whitepawn5 =
  List.find
    (fun x -> if Piece.get_name x = "whitepawn5" then true else false)
    (Piece.pieces jsonfile)

let whitepawn6 =
  List.find
    (fun x -> if Piece.get_name x = "whitepawn6" then true else false)
    (Piece.pieces jsonfile)

let whitepawn7 =
  List.find
    (fun x -> if Piece.get_name x = "whitepawn7" then true else false)
    (Piece.pieces jsonfile)

let whitepawn8 =
  List.find
    (fun x -> if Piece.get_name x = "whitepawn8" then true else false)
    (Piece.pieces jsonfile)

let whiterook1 =
  List.find
    (fun x -> if Piece.get_name x = "whiterook1" then true else false)
    (Piece.pieces jsonfile)

let whiteknight1 =
  List.find
    (fun x -> if Piece.get_name x = "whiteknight1" then true else false)
    (Piece.pieces jsonfile)

let whitebishop1 =
  List.find
    (fun x -> if Piece.get_name x = "whitebishop1" then true else false)
    (Piece.pieces jsonfile)

let whitequeen =
  List.find
    (fun x -> if Piece.get_name x = "whitequeen" then true else false)
    (Piece.pieces jsonfile)

let whiteking =
  List.find
    (fun x -> if Piece.get_name x = "whiteking" then true else false)
    (Piece.pieces jsonfile)

let whitebishop2 =
  List.find
    (fun x -> if Piece.get_name x = "whitebishop2" then true else false)
    (Piece.pieces jsonfile)

let whiteknight2 =
  List.find
    (fun x -> if Piece.get_name x = "whiteknight2" then true else false)
    (Piece.pieces jsonfile)

let whiterook2 =
  List.find
    (fun x -> if Piece.get_name x = "whiterook2" then true else false)
    (Piece.pieces jsonfile)

let row8 =
  [|
    Some blackrook1;
    Some blackknight1;
    Some blackbishop1;
    Some blackqueen;
    Some blackking;
    Some blackbishop2;
    Some blackknight2;
    Some blackrook2;
  |]

let row7 =
  [|
    Some blackpawn1;
    Some blackpawn2;
    Some blackpawn3;
    Some blackpawn4;
    Some blackpawn5;
    Some blackpawn6;
    Some blackpawn7;
    Some blackpawn8;
  |]

let make_empty_row () =
  [| None; None; None; None; None; None; None; None |]

let row6 = make_empty_row ()
let row5 = make_empty_row ()
let row4 = make_empty_row ()
let row3 = make_empty_row ()

let row2 =
  [|
    Some whitepawn1;
    Some whitepawn2;
    Some whitepawn3;
    Some whitepawn4;
    Some whitepawn5;
    Some whitepawn6;
    Some whitepawn7;
    Some whitepawn8;
  |]

let row1 =
  [|
    Some whiterook1;
    Some whiteknight1;
    Some whitebishop1;
    Some whitequeen;
    Some whiteking;
    Some whitebishop2;
    Some whiteknight2;
    Some whiterook2;
  |]

let sep = "----------------------------------------------------------"
let lett = "    a      b      c      d      e      f      g      h    "

let start_board =
  {
    grid = [| row1; row2; row3; row4; row5; row6; row7; row8 |];
    graveyard = [];
    model = model_move;
  }

let print_piece (piece : piece option) =
  match piece with
  | None -> print_string "|      "
  | Some piece -> print_string piece.rep

let print_piecerow (elt : piecerow) =
  Array.iter (fun x -> print_piece x) elt

let rec print_board ?(cycle = 8) grd =
  if cycle = 0 then (
    print_endline sep;
    print_endline lett)
  else
    let decr_cycle = cycle - 1 in
    print_endline sep;
    Array.get grd decr_cycle |> print_piecerow;
    cycle |> string_of_int |> ( ^ ) "|  " |> print_endline;
    print_board grd ~cycle:decr_cycle

let bk = ref blackking
let wk = ref whiteking