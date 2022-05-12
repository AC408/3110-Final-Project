open Piece

(**[piecerow] represents the row and columns of the chess board*)
type piecerow = piece option array

(**[board] represents the lines and rows of the chessboard*)
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

(** [row [number]] represents the corresponding horizontal row of the 
8-row chess board*)
val row8 : piecerow
val row7 : piecerow
val row6 : piecerow
val row5 : piecerow
val row4 : piecerow
val row3 : piecerow
val row2 : piecerow
val row1 : piecerow

(**[start_board] represents the lines and columns for the chess board*)
val start_board : board

(**[print_piece] returns the current representations of each space on the 
chessboard, either an empty space or a visual depiction of the piece*)
val print_piece: piece option -> unit

(**[print_piecerow] iterates through the array and prints every piece in the 
row*)
val print_piecerow : piecerow -> unit

(**[print_board] prints all pieces, dividing lines, and identification markers*)
val print_board : board -> unit
