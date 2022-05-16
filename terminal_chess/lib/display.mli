open Piece

type piecerow = piece option array
(**[piecerow] represents the row and columns of the chess board*)

type board = {
  grid : piecerow array;
  graveyard : string list;
  model : Board.model;
}
(**[board] represents the lines and rows of the chessboard*)

val row8 : piecerow
(** [row \[number\]] represents the corresponding horizontal row of the
    8-row chess board*)

val row7 : piecerow

val row6 : piecerow

val row5 : piecerow

val row4 : piecerow

val row3 : piecerow

val row2 : piecerow

val row1 : piecerow

val start_board : board
(**[start_board] represents the lines and columns for the chess board*)

val print_piece : piece option -> unit
(**[print_piece] returns the current representations of each space on
   the chessboard, either an empty space or a visual depiction of the
   piece*)

val print_piecerow : piecerow -> unit
(**[print_piecerow] iterates through the array and prints every piece in
   the row*)

val print_board : ?cycle:int -> piecerow array -> unit
(**[print_board] prints all pieces, dividing lines, and identification
   markers*)
