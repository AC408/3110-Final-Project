open Piece

type model = {
  moves : int;
  turn : Piece.color;
}

let model_move = { moves = 1; turn = White }

type change = Change

let get_turn (board : model) =
  match board.turn with
  | Black -> "Black"
  | White -> "White"

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
