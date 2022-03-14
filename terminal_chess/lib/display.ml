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

let row5 = {d1 = div; c_a = " "; d2 = div; c_b = " ";
d3 = div; c_c = " "; d4 = div; c_d = " ";
d5 = div; c_e = " "; d6 = div; c_f = " ";
d7 = div; c_g = " "; d8 = div; c_h = " ";
d9 = div;  }

let row4 = {d1 = div; c_a = " "; d2 = div; c_b = " ";
d3 = div; c_c = " "; d4 = div; c_d = " ";
d5 = div; c_e = " "; d6 = div; c_f = " ";
d7 = div; c_g = " "; d8 = div; c_h = " ";
d9 = div;  }

let row3 = {d1 = div; c_a = " "; d2 = div; c_b = " ";
d3 = div; c_c = " "; d4 = div; c_d = " ";
d5 = div; c_e = " "; d6 = div; c_f = " ";
d7 = div; c_g = " "; d8 = div; c_h = " ";
d9 = div;  }

let row2 = {d1 = div; c_a = whitepawn1.rep; d2 = div; c_b = whitepawn2.rep;
d3 = div; c_c = whitepawn3.rep; d4 = div; c_d = whitepawn4.rep;
d5 = div; c_e = whitepawn5.rep; d6 = div; c_f = whitepawn6.rep;
d7 = div; c_g = whitepawn7.rep; d8 = div; c_h = whitepawn8.rep;
d9 = div;  }

let row1 = {d1 = div; c_a = whiterook1.rep; d2 = div; c_b = whiteknight1.rep;
d3 = div; c_c = whitebishop1.rep; d4 = div; c_d = whitequeen.rep;
d5 = div; c_e = whiteking.rep; d6 = div; c_f = whitebishop2.rep;
d7 = div; c_g = whiteknight2.rep; d8 = div; c_h = whiterook2.rep;
d9 = div;  }

let start_board = {
  l1= "---------------------------------------";
  r8= row8;
  l2= "---------------------------------------";
  r7= row7;
  l3= "---------------------------------------";
  r6= row6;
  l4= "---------------------------------------";
  r5= row5;
  l5= "---------------------------------------";
  r4= row4;
  l6= "---------------------------------------";
  r3= row3;
  l7= "---------------------------------------";
  r2= row2;
  l8= "---------------------------------------";
  r1= row1;
  l9= "---------------------------------------";
}

let print_piecerow (elt : piecerow) =
  match elt with
  | {d1=d1; c_a=c_a; d2=d2; c_b=c_b; 
  d3=d3; c_c=c_c; d4=d4; c_d=c_d;
  d5=d5; c_e=c_e; d6=d6; c_f=c_f;
  d7=d7; c_g=c_g; d8=d8; c_h=c_h; d9=d9} -> 
    Printf.printf "%s\n%!" d1
    Printf.printf "%s\n%!" c_a
    Printf.printf "%s\n%!" d2
    Printf.printf "%s\n%!" c_b
    Printf.printf "%s\n%!" d3
    Printf.printf "%s\n%!" c_c
    Printf.printf "%s\n%!" d4
    Printf.printf "%s\n%!" c_d
    Printf.printf "%s\n%!" d5
    Printf.printf "%s\n%!" c_e
    Printf.printf "%s\n%!" row1.d6
    Printf.printf "%s\n%!" c_f
    Printf.printf "%s\n%!" d7
    Printf.printf "%s\n%!" c_g
    Printf.printf "%s\n%!" d8
    Printf.printf "%s\n%!" c_h
    Printf.printf "%s\n%!" d9
  | _ -> ()

let print_board (ex : board) =
  match ex with
  | {l1=l1; r8=r8; l2=l2; r7=r7; l3=l3; r6=r6; l4=l4; r5=r5; l5=l5; r4=r4; 
  l6=l6; r3=r3; l7=l7; r2=r2; l8=l8; r1=r1; l9=l9} -> 
    Printf.printf "%s\n%!" l1
    print_piecerow r8
    Printf.printf "%s\n%!" l2
    print_piecerow r7
    Printf.printf "%s\n%!" l3
    print_piecerow r6
    Printf.printf "%s\n%!" l4
    print_piecerow r5
    Printf.printf "%s\n%!" l5
    print_piecerow r4
    Printf.printf "%s\n%!" l6
    print_piecerow r3
    Printf.printf "%s\n%!" l7
    print_piecerow r2
    Printf.printf "%s\n%!" l8
    print_piecerow r1
    Printf.printf "%s\n%!" l9

  print_board start_board