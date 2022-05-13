open Display
open Piece

exception InvalidInput
exception EmptyCommand
exception InvalidQuit

(**[remove_blank] takes in a list of strings [strlist] and removes empty string elements.*)
val remove_blank : string list -> string list

(**[explode] takes in a string [str] and returns a list of chars equivalent to it.*)
val explode : string -> char list

(**[check_format] takes in a str and checks whether the 2 elements from check_valid_move is in the format
(a, b) and returns ab.*)
val check_format : char list -> string

(**[check_valid_move] takes in a string list [str] and if it started with move, it checks whether it has 2 more
elements for curr pos and next pos. It returns the 4-letter representation of a move from concating two
2-letter strings. Ex: (2,b) (3,b) -> 2b3b.*)
val check_valid_move : string list -> string

(**[check_quit] returns true if the string list [t] starts with “quit “ else false.*)
val check_quit : string -> bool 

(**[parse] takes in a string [str] and returns the string split based on empty space, and all empty space
removed.*)
val parse : string -> string list

(**[parse_mod] str takes in a string [str] and returns the string split based on empty space, with all empty
space removed, and then checks to see if it is a valid move.*)
val parse_mod : string -> string

(**[check1] is the row that corresponds to the input piece that the user selects.*)
val check1 : string -> piecerow

(**[check3] is the row that corresponds to the output space that the user selects.*)
val check3 : string -> piecerow

val rook_check : string -> bool
val bishop_check : string -> bool
val queen_check : string -> bool
val king_check : string -> bool
val knight_check : string -> bool
val pawn_check : string -> bool -> Piece.color -> bool
val castle: piece option -> string -> piece option -> bool
val check_piece : piece option -> string -> piece option -> bool 
val color_checker : piece option -> piece option -> string -> bool
val promote_pawn : string -> piece option -> bool

val incheck : piece list -> piece -> bool
val checkmated : piece list -> piece list -> piece -> piecerow -> piecerow -> piecerow -> piecerow -> piecerow -> piecerow -> piecerow -> piecerow -> bool