open Piece

type t = piece list

type model = {
 moves : int;
 turn : color
}

let model_move = {
  moves = 1;
  turn = White
}

type change = Change

let update_turn (board : model) (change : change) =
  match change with
  | Change -> 
    let turn = begin
      match board.turn with
      | Black -> White
      | White -> Black
    end in
    let moves = board.moves + 1 in {turn = turn; moves = moves}

    

  let blackrook1 = place_piece (Some ('a', 8)) Black Rook
  let blackknight1 = place_piece (Some ('b', 8)) Black Knight
  let blackbishop1 = place_piece (Some ('c', 8)) Black Bishop
  let blackqueen = place_piece (Some ('d', 8)) Black Queen
  let blackking = place_piece (Some ('e', 8)) Black King
  let blackbishop2 = place_piece (Some ('f', 8)) Black Bishop
  let blackknight2 = place_piece (Some ('g', 8)) Black Knight
  let blackrook2 = place_piece (Some ('h', 8)) Black Rook
  let blackpawn1 = place_piece (Some ('a', 7)) Black Pawn
  let blackpawn2 = place_piece (Some ('b', 7)) Black Pawn
  let blackpawn3 = place_piece (Some ('c', 7)) Black Pawn
  let blackpawn4 = place_piece (Some ('d', 7)) Black Pawn
  let blackpawn5 = place_piece (Some ('e', 7)) Black Pawn
  let blackpawn6 = place_piece (Some ('f', 7)) Black Pawn
  let blackpawn7 = place_piece (Some ('g', 7)) Black Pawn
  let blackpawn8 = place_piece (Some ('h', 7)) Black Pawn
  let whiterook1 = place_piece (Some ('a', 1)) White Rook
  let whiteknight1 = place_piece (Some ('b', 1)) White Knight
  let whitebishop1 = place_piece (Some ('c', 1)) White Bishop
  let whitequeen = place_piece (Some ('d', 1)) White Queen
  let whiteking = place_piece (Some ('e', 1)) White King
  let whitebishop2 = place_piece (Some ('f', 1)) White Bishop
  let whiteknight2 = place_piece (Some ('g', 1)) White Knight
  let whiterook2 = place_piece (Some ('h', 1)) White Rook
  let whitepawn1 = place_piece (Some ('a', 2)) White Pawn
  let whitepawn2 = place_piece (Some ('b', 2)) White Pawn
  let whitepawn3 = place_piece (Some ('c', 2)) White Pawn
  let whitepawn4 = place_piece (Some ('d', 2)) White Pawn
  let whitepawn5 = place_piece (Some ('e', 2)) White Pawn
  let whitepawn6 = place_piece (Some ('f', 2)) White Pawn
  let whitepawn7 = place_piece (Some ('g', 2)) White Pawn
  let whitepawn8 = place_piece (Some ('h', 2)) White Pawn
let init_board = [ blackrook1; blackrook2; blackbishop1; blackbishop2; 
blackknight1; blackknight2; blackqueen; blackking; blackpawn1; blackpawn2;
blackpawn3; blackpawn4; blackpawn5; blackpawn6; blackpawn7; blackpawn8; 
whiterook1; whiterook2; whitebishop1; whitebishop2; whiteknight1; whiteknight2; 
whitequeen; whiteking; whitepawn1; whitepawn2; whitepawn3; whitepawn4; 
whitepawn5; whitepawn6; whitepawn7; whitepawn8; ]