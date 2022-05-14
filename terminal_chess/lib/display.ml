open Piece
open Board

type piecerow = piece option array

type board = {
  l1 : string;
  r8 : piecerow;
  l2 : string;
  r7 : piecerow;
  l3 : string;
  r6 : piecerow;
  l4 : string;
  r5 : piecerow;
  l5 : string;
  r4 : piecerow;
  l6 : string;
  r3 : piecerow;
  l7 : string;
  r2 : piecerow;
  l8 : string;
  r1 : piecerow;
  l9 : string;
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

let row6 = [| None; None; None; None; None; None; None; None |]
let row5 = [| None; None; None; None; None; None; None; None |]
let row4 = [| None; None; None; None; None; None; None; None |]
let row3 = [| None; None; None; None; None; None; None; None |]

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
    l1 = sep;
    r8 = row8;
    l2 = sep;
    r7 = row7;
    l3 = sep;
    r6 = row6;
    l4 = sep;
    r5 = row5;
    l5 = sep;
    r4 = row4;
    l6 = sep;
    r3 = row3;
    l7 = sep;
    r2 = row2;
    l8 = sep;
    r1 = row1;
    l9 = sep;
    graveyard = [];
    model = model_move;
  }

let print_piece (piece : piece option) =
  match piece with
  | None -> print_string "|      "
  | Some piece -> print_string piece.rep

let print_piecerow (elt : piecerow) =
  Array.iter (fun x -> print_piece x) elt

let print_board (ex : board) =
  print_endline ex.l1;
  print_piecerow ex.r8;
  print_endline "|  8";
  print_endline ex.l2;
  print_piecerow ex.r7;
  print_endline "|  7";
  print_endline ex.l3;
  print_piecerow ex.r6;
  print_endline "|  6";
  print_endline ex.l4;
  print_piecerow ex.r5;
  print_endline "|  5";
  print_endline ex.l5;
  print_piecerow ex.r4;
  print_endline "|  4";
  print_endline ex.l6;
  print_piecerow ex.r3;
  print_endline "|  3";
  print_endline ex.l7;
  print_piecerow ex.r2;
  print_endline "|  2";
  print_endline ex.l8;
  print_piecerow ex.r1;
  print_endline "|  1";
  print_endline ex.l9;
  print_endline lett
