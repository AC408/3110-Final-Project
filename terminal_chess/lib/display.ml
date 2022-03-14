open Piece
open Board

type piecerow = {
  d1: string;
  c_a: piece option;
  d2: string;
  c_b: piece option;
  d3: string;
  c_c: piece option;
  d4: string;
  c_d: piece option;
  d5: string;
  c_e: piece option;
  d6: string;
  c_f: piece option;
  d7: string;
  c_g: piece option;
  d8: string;
  c_h: piece option;
  d9: string
}

type board = {
  l1: string;
  r8: piecerow;
  l2: string;
  r7: piecerow;
  l3: string;
  r6: piecerow;
  l4: string;
  r5: piecerow;
  l5: string;
  r4: piecerow;
  l6: string;
  r3: piecerow;
  l7: string;
  r2: piecerow;
  l8: string;
  r1: piecerow;
  l9: string
}

let div = "  | "
let div_l = " | "
let row8 = {d1 = div_l; c_a = Some blackrook1; d2 = div; c_b = Some blackknight1;
d3 = div; c_c = Some blackbishop1; d4 = div; c_d = Some blackqueen;
d5 = div; c_e = Some blackking; d6 = div; c_f = Some blackbishop2;
d7 = div; c_g = Some blackknight2; d8 = div; c_h = Some blackrook2;
d9 = div;  }

let row7 = {d1 = div_l; c_a = Some blackpawn1; d2 = div; c_b = Some blackpawn2;
d3 = div; c_c = Some blackpawn3; d4 = div; c_d = Some blackpawn4;
d5 = div; c_e = Some blackpawn5; d6 = div; c_f = Some blackpawn6;
d7 = div; c_g = Some blackpawn7; d8 = div; c_h = Some blackpawn8;
d9 = div;  }

let row6 = {d1 = div_l; c_a = None; d2 = div; c_b = None;
d3 = div; c_c = None; d4 = div; c_d = None;
d5 = div; c_e = None; d6 = div; c_f = None;
d7 = div; c_g = None; d8 = div; c_h = None;
d9 = div;  }

let row5 = {d1 = div_l; c_a = None; d2 = div; c_b = None;
d3 = div; c_c = None; d4 = div; c_d = None;
d5 = div; c_e = None; d6 = div; c_f = None;
d7 = div; c_g = None; d8 = div; c_h = None;
d9 = div;  }

let row4 = {d1 = div_l; c_a = None; d2 = div; c_b = None;
d3 = div; c_c = None; d4 = div; c_d = None;
d5 = div; c_e = None; d6 = div; c_f = None;
d7 = div; c_g = None; d8 = div; c_h = None;
d9 = div;  }

let row3 = {d1 = div_l; c_a = None; d2 = div; c_b = None;
d3 = div; c_c = None; d4 = div; c_d = None;
d5 = div; c_e = None; d6 = div; c_f = None;
d7 = div; c_g = None; d8 = div; c_h = None;
d9 = div;  }

let row2 = {d1 = div_l; c_a = Some whitepawn1; d2 = div; c_b = Some whitepawn2;
d3 = div; c_c = Some whitepawn3; d4 = div; c_d = Some whitepawn4;
d5 = div; c_e = Some whitepawn5; d6 = div; c_f = Some whitepawn6;
d7 = div; c_g = Some whitepawn7; d8 = div; c_h = Some whitepawn8;
d9 = div;  }

let row1 = {d1 = div_l; c_a = Some whiterook1; d2 = div; c_b = Some whiteknight1;
d3 = div; c_c = Some whitebishop1; d4 = div; c_d = Some whitequeen;
d5 = div; c_e = Some whiteking; d6 = div; c_f = Some whitebishop2;
d7 = div; c_g = Some whiteknight2; d8 = div; c_h = Some whiterook2;
d9 = div;  }

let start_board = {
  l1= "-------------------------------------------";
  r8= row8;
  l2= "-------------------------------------------";
  r7= row7;
  l3= "-------------------------------------------";
  r6= row6;
  l4= "-------------------------------------------";
  r5= row5;
  l5= "-------------------------------------------";
  r4= row4;
  l6= "-------------------------------------------";
  r3= row3;
  l7= "-------------------------------------------";
  r2= row2;
  l8= "-------------------------------------------";
  r1= row1;
  l9= "-------------------------------------------";
}

let print_piece (piece: piece option) = begin
  match piece with 
  | None -> print_string " "
  | Some piece -> print_string piece.rep
end

let print_piecerow (elt : piecerow) = begin
  print_string elt.d1;
  print_piece elt.c_a;
  print_string elt.d2;
  print_piece elt.c_b;
  print_string elt.d3;
  print_piece elt.c_c;
  print_string elt.d4;
  print_piece elt.c_d;
  print_string elt.d5;
  print_piece elt.c_e;
  print_string elt.d6;
  print_piece elt.c_f;
  print_string elt.d7;
  print_piece elt.c_g;
  print_string elt.d8;
  print_piece elt.c_h;
  print_endline elt.d9;
end

 
let print_board (ex : board) = begin
  print_endline ex.l1; 
  print_piecerow ex.r8;
  print_endline ex.l2;
  print_piecerow ex.r7;
  print_endline ex.l3;
  print_piecerow ex.r6;
  print_endline ex.l4;
  print_piecerow ex.r5;
  print_endline ex.l5;
  print_piecerow ex.r4;
  print_endline ex.l6;
  print_piecerow ex.r3;
  print_endline ex.l7;
  print_piecerow ex.r2;
  print_endline ex.l8;
  print_piecerow ex.r1;
  print_endline ex.l9;
end

