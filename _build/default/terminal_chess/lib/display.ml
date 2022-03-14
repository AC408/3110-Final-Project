open Piece
open Board

type piecerow = {
  d1: string;
  c_a: string;
  d2: string;
  c_b: string;
  d3: string;
  c_c: string;
  d4: string;
  c_d: string;
  d5: string;
  c_e: string;
  d6: string;
  c_f: string;
  d7: string;
  c_g: string;
  d8: string;
  c_h: string;
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

let div = "|"
let row8 = {d1 = div; c_a = blackrook1.rep; d2 = div; c_b = blackknight1.rep;
d3 = div; c_c = blackbishop1.rep; d4 = div; c_d = blackqueen.rep;
d5 = div; c_e = blackking.rep; d6 = div; c_f = blackbishop2.rep;
d7 = div; c_g = blackknight2.rep; d8 = div; c_h = blackrook2.rep;
d9 = div;  }

let row7 = {d1 = div; c_a = blackpawn1.rep; d2 = div; c_b = blackpawn2.rep;
d3 = div; c_c = blackpawn3.rep; d4 = div; c_d = blackpawn4.rep;
d5 = div; c_e = blackpawn5.rep; d6 = div; c_f = blackpawn6.rep;
d7 = div; c_g = blackpawn7.rep; d8 = div; c_h = blackpawn8.rep;
d9 = div;  }

let row6 = {d1 = div; c_a = " "; d2 = div; c_b = " ";
d3 = div; c_c = " "; d4 = div; c_d = " ";
d5 = div; c_e = " "; d6 = div; c_f = " ";
d7 = div; c_g = " "; d8 = div; c_h = " ";
d9 = div;  }