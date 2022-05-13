open Piece

type model = {
  moves : int;
  turn : Piece.color;
}

let model_move = { moves = 1; turn = White }

type change = Change

let update_turn (board : model) (change : change) =
  match change with
  | Change ->
      let turn =
        match board.turn with
        | Black -> White
        | White -> Black
      in
      let moves = board.moves + 1 in
      { turn; moves }

let blackrook1 = place_piece (Some ('a', 8)) Black Rook "|  ♜   " false

let blackknight1 =
  place_piece (Some ('b', 8)) Black Knight "|  ♞   " false

let blackbishop1 =
  place_piece (Some ('c', 8)) Black Bishop "|  ♝   " false

let blackqueen = place_piece (Some ('d', 8)) Black Queen "|  ♛   " false

let blackking = place_piece (Some ('e', 8)) Black King "|  ♚   " false

let blackbishop2 =
  place_piece (Some ('f', 8)) Black Bishop "|  ♝   " false

let blackknight2 =
  place_piece (Some ('g', 8)) Black Knight "|  ♞   " false

let blackrook2 = place_piece (Some ('h', 8)) Black Rook "|  ♜   " false

let blackpawn1 = place_piece (Some ('a', 7)) Black Pawn "|  ♟   " false

let blackpawn2 = place_piece (Some ('b', 7)) Black Pawn "|  ♟   " false

let blackpawn3 = place_piece (Some ('c', 7)) Black Pawn "|  ♟   " false

let blackpawn4 = place_piece (Some ('d', 7)) Black Pawn "|  ♟   " false

let blackpawn5 = place_piece (Some ('e', 7)) Black Pawn "|  ♟   " false

let blackpawn6 = place_piece (Some ('f', 7)) Black Pawn "|  ♟   " false

let blackpawn7 = place_piece (Some ('g', 7)) Black Pawn "|  ♟   " false

let blackpawn8 = place_piece (Some ('h', 7)) Black Pawn "|  ♟   " false

let whiterook1 = place_piece (Some ('a', 1)) White Rook "|  ♖   " false

let whiteknight1 =
  place_piece (Some ('b', 1)) White Knight "|  ♘   " false

let whitebishop1 =
  place_piece (Some ('c', 1)) White Bishop "|  ♗   " false

let whitequeen = place_piece (Some ('d', 1)) White Queen "|  ♕   " false

let whiteking = place_piece (Some ('e', 1)) White King "|  ♔   " false

let whitebishop2 =
  place_piece (Some ('f', 1)) White Bishop "|  ♗   " false

let whiteknight2 =
  place_piece (Some ('g', 1)) White Knight "|  ♘   " false

let whiterook2 = place_piece (Some ('h', 1)) White Rook "|  ♖   " false

let whitepawn1 = place_piece (Some ('a', 2)) White Pawn "|  ♙   " false

let whitepawn2 = place_piece (Some ('b', 2)) White Pawn "|  ♙   " false

let whitepawn3 = place_piece (Some ('c', 2)) White Pawn "|  ♙   " false

let whitepawn4 = place_piece (Some ('d', 2)) White Pawn "|  ♙   " false

let whitepawn5 = place_piece (Some ('e', 2)) White Pawn "|  ♙   " false

let whitepawn6 = place_piece (Some ('f', 2)) White Pawn "|  ♙   " false

let whitepawn7 = place_piece (Some ('g', 2)) White Pawn "|  ♙   " false

let whitepawn8 = place_piece (Some ('h', 2)) White Pawn "|  ♙   " false
