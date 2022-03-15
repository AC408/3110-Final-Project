open Display
open Piece

type t

exception Quit
exception InvalidInput
exception EmptyCommand
exception InvalidQuit

(** parses the command that a player makes in game. Returns a 4 letter string that represents the current row, current column, and next row, new column that a piece will move to. 
raises Quit if the player types <quit>. raises InvalidQuit if the player types <quit> followed by other chars. raises EmptyCommand if the input is an empty string. raises InvalidInput if the player types in an invalid input.
  An input is invalid if it is not <quit> or <move (a,b) (c,d)> where a, b, c, and d are one character elements. There can be as many spaces in front and after every command and between move and ( and between ) and ( *)
val parse : string -> string
val check1 : string -> piecerow
val check2 : string -> piece option
val check3 : string -> piecerow
val check4 : string -> piece option
