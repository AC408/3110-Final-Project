(** How a chess piece will be represented. *)

type level =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

(** Level of each chess piece *)

type color =
  | White
  | Black

(** Color of each chess piece *)

type position = {
  row : int;
  col : string;
}
(** Position of each chess piece *)

exception NoPiece
(** Raised when no piece found *)

exception NoPosition
(** Raised when no position found *)

type piece = {
  name : string;
  position : position;
  color : string;
  level : string;
  rep : string;
  moved : string;
}
(** Characteristics of each chess piece. *)

type t = { pieces : piece list }
(** type t is a board including all pieces*)

val pieces : t -> piece list
(** [pieces t] is the list of pieces that [t] represents.*)

val position_of_json : Yojson.Basic.t -> position
(** [position_of_json] takes in a JSON and returns the position.
    Requires: [j] is a valid JSON chess board representation. *)

val piece_of_json : Yojson.Basic.t -> piece
(** [position_of_json] takes in a JSON and returns the piece. Requires:
    [j] is a valid JSON chess board representation. *)

val t_from_json : Yojson.Basic.t -> t
(** [position_of_json] takes in a JSON and returns the board. Requires:
    [j] is a valid JSON chess board representation. *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the set of pieces that [j] represents. Requires:
    [j] is a valid JSON chess board representation. *)

val rep : piece option -> string
(** [get_rep p] returns the rep of the piece option [p]. Raises NoPiece
    if p is None*)

val get_level : piece -> level
(** [get_level p] returns the level of the piece [p]. *)

val get_color : piece -> color
(** [get_color p] returns the color of the piece [p]. *)

val get_position : piece -> position
(** [get_position p] returns the position of the piece [p]. *)

val string_of_pos : piece -> string
(** [get_position p] returns the position of the piece [p]. Raises
    NoPosition if position is None *)

val get_name : piece -> string
(** [get_rep p] returns the name of the piece [p] *)

val get_rep : piece -> string
(** [get_rep p] returns the rep of the piece [p] *)

val place_piece :
  string -> position -> string -> string -> string -> string -> piece
(** [place_piece pos c l] returns the position [pos], color [c], level
    [l], and representation [rep] of the function. *)

val have_moved : piece -> bool
(** [have_moved p] returns the true if the piece [p] have been moved and
    false otherwise. *)
