(** functions to update a board and specific pieces that go on the board*)

type model = {
  moves : int;
  turn : Piece.color;
}
(**representation of the current board*)

val model_move : model
(** model value of the starting board of each game *)

type change =
  | Change
      (** sends a message to the model to update the color and number of
          moves *)

val get_turn : model -> string
(** returns a string of whose turn it currently is for model 'board' *)

val update_turn : model -> change -> model
(** changes the player turn from White to Black (or vice versa). Adds 1
    to the number of moves.*)
