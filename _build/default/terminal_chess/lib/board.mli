(** functions to update a board and specific pieces that go on the board*)

(**representation of the current board*)
type model = {
  moves : int;
  turn : Piece.color
 }

(**model value of the starting board of each game*)
val model_move : model

(**sends a message to the model to update the color and number of moves*)
type change = Change

(** changes the player turn from White to Black (or vice versa). Adds 1 to the number of moves.*)
val update_turn : model -> change -> model

(**Representations of all the pieces that are found on the starting board before any captures*)
val blackrook1 : Piece.piece
val blackknight1 : Piece.piece
val blackbishop1 : Piece.piece
val blackqueen : Piece.piece
val blackking : Piece.piece
val blackbishop2 : Piece.piece
val blackknight2 : Piece.piece
val blackrook2 : Piece.piece
val blackpawn1 : Piece.piece
val blackpawn2 : Piece.piece
val blackpawn3 : Piece.piece
val blackpawn4 : Piece.piece
val blackpawn5 : Piece.piece
val blackpawn6 : Piece.piece
val blackpawn7 : Piece.piece
val blackpawn8 : Piece.piece
val whiterook1 : Piece.piece
val whiteknight1 : Piece.piece
val whitebishop1 : Piece.piece
val whitequeen : Piece.piece
val whiteking : Piece.piece
val whitebishop2 : Piece.piece
val whiteknight2 : Piece.piece
val whiterook2 : Piece.piece
val whitepawn1 : Piece.piece
val whitepawn2 : Piece.piece
val whitepawn3 : Piece.piece
val whitepawn4 : Piece.piece
val whitepawn5 : Piece.piece
val whitepawn6 : Piece.piece
val whitepawn7 : Piece.piece
val whitepawn8 : Piece.piece

