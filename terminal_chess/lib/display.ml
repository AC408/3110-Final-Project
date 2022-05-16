open Piece
open Board

type piecerow = piece option array

type board = {
  grid : piecerow array;
  graveyard : string list;
  model : model;
}

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

let rec print_board ?(cycle = 8) brd =
  if cycle = 0 then (
    print_endline sep;
    print_endline lett)
  else
    let decr_cycle = cycle - 1 in
    print_endline sep;
    Array.get brd.grid decr_cycle |> print_piecerow;
    cycle |> string_of_int |> ( ^ ) "|  " |> print_endline;
    print_board brd ~cycle:decr_cycle
