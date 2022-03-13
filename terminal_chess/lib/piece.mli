(** How a chess piece will be represented. *)

(** Level of each chess piece *)
type level = Pawn | Knight | Bishop | Rook | Queen | King
type color = White | Black
type position = (char * int) option

(** Characteristics of each chess piece. *)
type piece

(** [get_level p] returns the level of the piece [p]. *)
val get_level : piece -> level

(** [get_color p] returns the color of the piece [p]. *)
val get_color : piece -> color

(** [get_position p] returns the position of the piece [p]. *)
val get_position : piece -> (char * int) option

(** [place_piece pos c l] returns the position [pos], color [c] and level [l] of the function. *)
val place_piece : (char * int) option -> color -> level -> piece

(** [move_piece p pos] moves the piece [p] to position [pos]. *)  
val move_piece : piece -> (char* int) option -> piece

(** [capture_piece p] changes the position of the piece after it has been captured. *)
val capture_piece : piece -> piece
