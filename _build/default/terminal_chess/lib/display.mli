open Piece

type piecerow = piece option array
(**[piecerow] represents the row and columns of the chess board*)

type board = {
  grid : piecerow array;
  graveyard : string list;
  model : Board.model;
}
(**[board] represents the lines and rows of the chessboard*)

val blackqueen : piece
val whitequeen : piece
val blackking : piece
val blackking : piece
val blackbishop1 : piece
val blackbishop2 : piece
val whitebishop1 : piece
val whitebishop2 : piece
val blackrook1 : piece
val blackrook2 : piece
val whiterook1 : piece
val whiterook2 : piece
val blackknight1 : piece
val blackknight2 : piece
val whiteknight1 : piece
val whiteknight2 : piece
val blackpawn1 : piece
val blackpawn2 : piece
val blackpawn3 : piece
val blackpawn4 : piece
val blackpawn5 : piece
val blackpawn6 : piece
val blackpawn7 : piece
val blackpawn8 : piece
val whitepawn1 : piece
val whitepawn2 : piece
val whitepawn3 : piece
val whitepawn4 : piece
val whitepawn5 : piece
val whitepawn6 : piece
val whitepawn7 : piece

val whitepawn8 : piece
(**The above values represent the pieces that are listed on the starting
   board.*)

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

val wk : piece ref
val bk : piece ref
val make_empty_row : unit -> piecerow