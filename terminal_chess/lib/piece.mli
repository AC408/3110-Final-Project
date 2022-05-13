(** How a chess piece will be represented. *)

(** Level of each chess piece *)
type level =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

type color =
  | White
  | Black

type position = (char * int) option

exception NoPiece

type piece = {
  position : position;
  color : color;
  level : level;
  rep : string;
  moved : bool;
}
(** Characteristics of each chess piece. *)

val rep : piece option -> string

val get_level : piece -> level
(** [get_level p] returns the level of the piece [p]. *)

val get_color : piece -> color
(** [get_color p] returns the color of the piece [p]. *)

val get_position : piece -> (char * int) option
(** [get_position p] returns the position of the piece [p]. *)

val get_rep : piece -> string
(** [get_rep p] returns the rep of the piece [p] *)

val place_piece :
  (char * int) option -> color -> level -> string -> bool -> piece
(** [place_piece pos c l] returns the position [pos], color [c], level
    [l], and representation [rep] of the function. *)

val have_moved : piece -> bool
(** [have_moved p] returns the true if the piece [p] have been moved and
    false otherwise. *)
