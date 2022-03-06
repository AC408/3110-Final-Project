(** How a chess piece will be represented. *)

(** Level of each chess piece *)
type level = Pawn | Knight | Bishop | Rook | Queen | King
type color = White | Black
type position = (char, int)

(** Characteristics of each chess piece *)
type piece

(** [get_level p] returns the level of the piece [p]. *)
val get_level (piece -> unknown)

(** [get_color p] returns the color of the piece [p]. *)
val get_color (piece -> unknown)

(** [get_position p] returns the position of the piece [p]. *)
val get_position (piece -> unknown)

(** [place_piece pos c l] returns the posiiton [pos], color [c] and level [l] of the function. *)
val place_piece pos c l (unknown -> unknown)

(** [move_piece p pos] moves the piece [p] to position [pos]. *)  
val move_piece p pos 
