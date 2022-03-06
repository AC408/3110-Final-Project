open Piece

type t = piece list

let init_board = [
  blackrook1 = { position = ('a', 8) ; color = Black ; level = Rook}
  blackknight1 = { position = ('b', 8) ; color = Black; level = Knight}
  blackbishop1 = { position = ('c', 8) ; color = Black; level = Bishop}
  blackqueen = { position = ('d', 8) ; color = Black; level = Queen}
  blackking = { position = ('e', 8) ; color = Black; level = King}
  blackbishop2 = { position = ('f', 8) ; color = Black; level = Bishop}
  blackknight2 = { position = ('g', 8) ; color = Black; level = Knight}
  blackrook2 = { position = ('h', 8) ; color = Black; level = Rook}
  blackpawn1 = { position = ('a', 7) ; color = Black; level = Pawn}
  blackpawn2 = { position = ('b', 7) ; color = Black; level = Pawn}
  blackpawn3 = { position = ('c', 7) ; color = Black; level = Pawn}
  blackpawn4 = { position = ('d', 7) ; color = Black; level = Pawn}
  blackpawn5 = { position = ('e', 7) ; color = Black; level = Pawn}
  blackpawn6 = { position = ('f', 7) ; color = Black; level = Pawn}
  blackpawn7 = { position = ('g', 7) ; color = Black; level = Pawn}
  blackpawn8 = { position = ('h', 7) ; color = Black; level = Pawn}
  whiterook1 = { position = ('a', 1) ; color = White; level = Rook}
  whiteknight1 = { position = ('b', 1) ; color = White; level = Knight}
  whitebishop1 = { position = ('c', 1) ; color = White; level = Bishop}
  whitequeen = { position = ('d', 1) ; color = White; level = Queen}
  whiteking = { position = ('e', 1) ; color = White; level = King}
  whitebishop2 = { position = ('f', 1) ; color = White; level = Bishop}
  whiteknight2 = { position = ('g', 1) ; color = White; level = Knight}
  whiterook2 = { position = ('h', 1) ; color = White; level = Rook}
  whitepawn1 = { position = ('a', 2) ; color = White; level = Pawn}
  whitepawn2 = { position = ('b', 2) ; color = White; level = Pawn}
  whitwpawn3 = { position = ('c', 3) ; color = White; level = Pawn}
  whitepawn4 = { position = ('d', 4) ; color = White; level = Pawn}
  whitepawn5 = { position = ('e', 5) ; color = White; level = Pawn}
  whitepawn6 = { position = ('f', 6) ; color = White; level = Pawn}
  whitepawn7 = { position = ('g', 7) ; color = White; level = Pawn}
  whitepawn8 = { position = ('h', 9) ; color = White; level = Pawn}
]
