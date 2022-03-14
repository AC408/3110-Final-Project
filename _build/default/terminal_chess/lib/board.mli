

type model = {
  moves : int;
  turn : Piece.color
 }

val model_move : model

type change = Change

val update_turn : model -> change -> model

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
