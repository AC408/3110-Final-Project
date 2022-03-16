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
<<<<<<< HEAD
val check3 : string -> piecerow
val check_quit : string -> bool

=======
val check2 : string -> piece option
val check3 : string -> piecerow
val check4 : string -> piece option
>>>>>>> bb3f582bd794488e640e73582a84ba4fd9c22424
