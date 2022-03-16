open Piece

type piecerow = piece option array

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
  l9: string;
  graveyard : string list;
  model : Board.model
}

val row8 : piecerow
val row7 : piecerow
val row6 : piecerow
val row5 : piecerow
val row4 : piecerow
val row3 : piecerow
val row2 : piecerow
val row1 : piecerow

val start_board : board

val print_piece: piece option -> unit
val print_piecerow : piecerow -> unit
val print_board : board -> unit
