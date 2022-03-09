open Piece

type t = piece list

type model = {
 moves : int;
 turn : color
}

let model_move = {
  moves = 1
  turn = White
}

type change = Change

let update_turn (board :t) (change : change) =
  match change with
  | Change -> 
    let turn = begin
      match board.turn with
      | Black -> White
      | White -> Black
    end in
    let moves = model.moves + 1 in {turn = turn; moves = moves}

 

  let blackrook1 = {position = ('a', 8) ; color = Black ; level = Rook}
  let blackknight1 = { position = ('b', 8) ; color = Black; level = Knight}
  let blackbishop1 = { position = ('c', 8) ; color = Black; level = Bishop}
  let blackqueen = { position = ('d', 8) ; color = Black; level = Queen}
  let blackking = { position = ('e', 8) ; color = Black; level = King}
  let blackbishop2 = { position = ('f', 8) ; color = Black; level = Bishop}
  let blackknight2 = { position = ('g', 8) ; color = Black; level = Knight}
  let blackrook2 = { position = ('h', 8) ; color = Black; level = Rook}
  let blackpawn1 = { position = ('a', 7) ; color = Black; level = Pawn}
  let blackpawn2 = { position = ('b', 7) ; color = Black; level = Pawn}
  let blackpawn3 = { position = ('c', 7) ; color = Black; level = Pawn}
  let blackpawn4 = { position = ('d', 7) ; color = Black; level = Pawn}
  let blackpawn5 = { position = ('e', 7) ; color = Black; level = Pawn}
  let blackpawn6 = { position = ('f', 7) ; color = Black; level = Pawn}
  let blackpawn7 = { position = ('g', 7) ; color = Black; level = Pawn}
  let blackpawn8 = { position = ('h', 7) ; color = Black; level = Pawn}
  let whiterook1 = { position = ('a', 1) ; color = White; level = Rook}
  let whiteknight1 = { position = ('b', 1) ; color = White; level = Knight}
  let whitebishop1 = { position = ('c', 1) ; color = White; level = Bishop}
  let whitequeen = { position = ('d', 1) ; color = White; level = Queen}
  let whiteking = { position = ('e', 1) ; color = White; level = King}
  let whitebishop2 = { position = ('f', 1) ; color = White; level = Bishop}
  let whiteknight2 = { position = ('g', 1) ; color = White; level = Knight}
  let whiterook2 = { position = ('h', 1) ; color = White; level = Rook}
  let whitepawn1 = { position = ('a', 2) ; color = White; level = Pawn}
  let whitepawn2 = { position = ('b', 2) ; color = White; level = Pawn}
  let whitepawn3 = { position = ('c', 2) ; color = White; level = Pawn}
  let whitepawn4 = { position = ('d', 2) ; color = White; level = Pawn}
  let whitepawn5 = { position = ('e', 2) ; color = White; level = Pawn}
  let whitepawn6 = { position = ('f', 2) ; color = White; level = Pawn}
  let whitepawn7 = { position = ('g', 2) ; color = White; level = Pawn}
  let whitepawn8 = { position = ('h', 2) ; color = White; level = Pawn}
let init_board = [ blackrook1; blackrook2; blackbishop1; blackbishop2; 
blackknight1; blackknight2; blackqueen; blackking; blackpawn1; blackpawn2;
blackpawn3; blackpawn4; blackpawn5; blackpawn6; blackpawn7; blackpawn8; 
whiterook1; whiterook2; whitebishop1; whitebishop2; whiteknight1; whiteknight2; 
whitequeen; whiteking; whitepawn1; whitepawn2; whitepawn3; whitepawn4; 
whitepawn5; whitepawn6; whitepawn7; whitepawn8; ]