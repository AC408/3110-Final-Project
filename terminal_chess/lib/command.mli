open Display
open Piece

exception InvalidInput
exception EmptyCommand
exception InvalidQuit

val remove_blank : string list -> string list
(**[remove_blank] takes in a list of strings [strlist] and removes empty
   string elements.*)

val explode : string -> char list
(**[explode] takes in a string [str] and returns a list of chars
   equivalent to it.*)

val check_format : char list -> string
(**[check_format] takes in a str and checks whether the 2 elements from
   check_valid_move is in the format (a, b) and returns ab.*)

val check_valid_move : string list -> string
(**[check_valid_move] takes in a string list [str] and if it started
   with move, it checks whether it has 2 more elements for curr pos and
   next pos. It returns the 4-letter representation of a move from
   concating two 2-letter strings. Ex: (2,b) (3,b) -> 2b3b.*)

val check_quit : string -> bool
(**[check_quit] returns true if the string list [t] starts with “quit “
   else false.*)

val parse : string -> string list
(**[parse] takes in a string [str] and returns the string split based on
   empty space, and all empty space removed.*)

val parse_mod : string -> string
(**[parse_mod] str takes in a string [str] and returns the string split
   based on empty space, with all empty space removed, and then checks
   to see if it is a valid move.*)

val check1 : string -> piecerow
(**[check1] is the row that corresponds to the input piece that the user
   selects.*)

val check3 : string -> piecerow
(**[check3] is the row that corresponds to the output space that the
   user selects.*)

val castle : piece option -> string -> piece option -> string * bool
(**[castle] takes in a piece option [i_p] representing the input
   position, a string representing the input, and another piece option
   representing the output position [o_p] and returns a boolean in
   accordance to whether the piece may or may not be moved.*)

val check_piece :
  piece option ->
  string ->
  piece option ->
  bool ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  bool
(**[check_piece] takes in a piece option, string, piece option, boolean,
   and several piecerows and returns whether the desired move is valid.*)

val color_checker : piece option -> piece option -> string -> bool
(**[color_checker] takes in a piece option representing the input
   position [i_p] as well as one representing the output position [o_p]
   as well as a string representing the input and returns a boolean
   based on whether the color is correct.*)

val promote_pawn : string -> piece option -> bool

val has_move :
  piece list ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  (piece * string) list

val incheck :
  piece list ->
  piece ->
  bool ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  bool

val checkmated :
  piece list ->
  piece list ->
  piece ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  piecerow ->
  bool
