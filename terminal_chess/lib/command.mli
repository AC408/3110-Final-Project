(** How user commands will be parsed and executed on *)

open Display
open Piece

exception InvalidInput
(** Raised when an invalid input is detected*)

exception EmptyCommand
(** Raised when command is empty*)

exception InvalidQuit
(** Raised when user wants to quit the game*)

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

val check3 : string -> piecerow array -> piecerow
(**[check3] is the row that corresponds to the output space that the
   user selects.*)

val check1 : string -> piecerow array -> piecerow
(**[check1] is the row that corresponds to the input piece that the user
   selects.*)

val castle :
  piece option ->
  string ->
  piece option ->
  piecerow array ->
  string * bool
(**[castle] takes in a piece option [i_p] representing the input
   position, a string representing the input, and another piece option
   representing the output position [o_p] and returns a boolean in
   accordance to whether the piece may or may not be moved.*)

val check_piece :
  piece option -> string -> piece option -> piecerow array -> bool
(**[check_piece] takes in a piece option, string, piece option, boolean,
   and several piecerows and returns whether the desired move is valid.*)

val color_checker :
  piece option -> piece option -> string -> piecerow array -> bool
(**[color_checker] takes in a piece option representing the input
   position [i_p] as well as one representing the output position [o_p]
   as well as a string representing the input and returns a boolean
   based on whether the color is correct.*)

val promote_pawn : string -> piece option -> bool
(**[promote_pawn] takes in a string [input] and piece option [i_p] and
   returns a boolean in accordance to whether the pawn can be moved.*)

val has_move :
  piece list ->
  piece list ->
  piece ->
  piecerow array ->
  (piece * string) list
(**[has_move] takes in a piece list called [w_plist] representing the
   list of white pieces, a piece list called [b_plist] representing the
   list of white pieces, a piece [king] representing the king piece, and
   a grid and it returns a (piece * string) list representing the list
   of pieces and its corresponding moves that a player can make
   disregarding the game state (check, etc). *)

val has_legal_move :
  (piece * string) list -> piece -> piecerow array -> bool
(**[has_legal_move] takes in a (piece * string) list representing the
   list of pieces and its corresponding moves, a piece [king]
   representing the king piece, a grid [grid], and returns whether these
   moves are valid*)

val incheck :
  piece list -> piece list -> piece -> piecerow array -> bool
(**[incheck] takes in a piece list called [w_plist] representing the
   list of white pieces, a piece list called [b_plist] representing the
   list of black pieces, a piece [king] representing the king piece, and
   a grid [grid] and it returns whether a player is in check.*)

val checkmated :
  piece list -> piece list -> piece -> piecerow array -> bool
(**[checkmated] takes in a piece list called [w_p_list] representing the
   list of white pieces, a piece list called [b_p_list] representing the
   list of black pieces, a piece [king] representing the king piece, and
   a grid [grid] and it returns whether a player is in checkmate. *)

val update_avail_lst :
  piece list ref -> piece list ref -> piece option array array -> unit
(**[update_avail_lst] takes in a piece list ref called [a_wp]
   representing the list of white pieces, a piece list ref called [a_bp]
   representing the list of black pieces, and updates a_wp and a_bp
   according to whether theres a piece of that color still on the board.
   It returns unit. *)

val print_list : string list -> unit
(**[print_list] takes in a list of strings and prints each element,
   separated by a space.*)

val color_matcher : board -> piece option -> bool
(**[color_matcher] takes in a piece option, returning an exception if
   there is no piece on the space in question. If there is a piece, it
   returns [true] if the color of the player whose turn it is is
   different from the color of the piece that is on the space.*)
