open Display

type t
(* 
exception Quit *)
exception InvalidInput
exception EmptyCommand
exception InvalidQuit


val parse : string -> string list
val parse_mod : string -> string
val check1 : string -> piecerow
val check3 : string -> piecerow
val check_quit : string -> bool

