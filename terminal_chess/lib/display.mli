(** How the chess elements will be displayed *)

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
(**[blackqueen] is the black queen*)

val whitequeen : piece
(**[whitequeen] is the white queen*)

val blackking : piece
(**[blackking] is the black king*)

val whiteking : piece
(**[whiteking] is the white king*)

val blackbishop1 : piece
(**[blackbishop1] is the black bishop with starting location c8 *)

val blackbishop2 : piece
(**[blackbishop2] is the black bishop with starting location f8*)

val whitebishop1 : piece
(**[whitebishop1] is the white bishop with starting location c1 *)

val whitebishop2 : piece
(**[whitebishop2] is the white bishop with starting location f1*)

val blackrook1 : piece
(**[blackrook1] is the black rook with starting location a8 *)

val blackrook2 : piece
(**[blackrook2] is the black rook with starting location h8*)

val whiterook1 : piece
(**[whiterook1] is the white rook with starting location a1*)

val whiterook2 : piece
(**[whiterook2] is the white rook with starting location h1*)

val blackknight1 : piece
(**[blackknight1] is the black knight with starting location b8*)

val blackknight2 : piece
(**[blackknight2] is the black knight with starting location g8*)

val whiteknight1 : piece
(**[whiteknight1] is the white knight with starting location b1*)

val whiteknight2 : piece
(**[whiteknight2] is the white knight with starting location g1*)

val blackpawn1 : piece
(**[blackpawn1] is the black pawn with starting location a7*)

val blackpawn2 : piece
(**[blackpawn2] is the black pawn with starting location b7*)

val blackpawn3 : piece
(**[blackpawn3] is the black pawn with starting location c7*)

val blackpawn4 : piece
(**[blackpawn4] is the black pawn with starting location d7*)

val blackpawn5 : piece
(**[blackpawn5] is the black pawn with starting location e7*)

val blackpawn6 : piece
(**[blackpawn6] is the black pawn with starting location f7*)

val blackpawn7 : piece
(**[blackpawn7] is the black pawn with starting location g7*)

val blackpawn8 : piece
(**[blackpawn8] is the black pawn with starting location h7*)

val whitepawn1 : piece
(**[whitepawn1] is the white pawn with starting location a2*)

val whitepawn2 : piece
(**[whitepawn2] is the white pawn with starting location b2*)

val whitepawn3 : piece
(**[whitepawn3] is the white pawn with starting location c2*)

val whitepawn4 : piece
(**[whitepawn4] is the white pawn with starting location d2*)

val whitepawn5 : piece
(**[whitepawn5] is the white pawn with starting location e2*)

val whitepawn6 : piece
(**[whitepawn6] is the white pawn with starting location f2*)

val whitepawn7 : piece
(**[whitepawn7] is the white pawn with starting location g2*)

val whitepawn8 : piece
(**[whitepawn8] is the white pawn with starting location h2*)

val row8 : piecerow
(** [row8] represents the 8th row of the chess board*)

val row7 : piecerow
(** [row7] represents the 7th row of the chess board*)

val row6 : piecerow
(** [row6] represents the 6th row of the chess board*)

val row5 : piecerow
(** [row5] represents the 5th row of the chess board*)

val row4 : piecerow
(** [row4] represents the 4th row of the chess board*)

val row3 : piecerow
(** [row3] represents the 3th row of the chess board*)

val row2 : piecerow
(** [row2] represents the 2th row of the chess board*)

val row1 : piecerow
(** [row1] represents the 1th row of the chess board*)

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
(**[wk] holds the piece that represents the white king*)

val bk : piece ref
(**[bk] holds the piece that represents the black king*)

val make_empty_row : unit -> piecerow
(**[make_empty_row] creates the representation array of a row that has
   no pieces on it.*)
