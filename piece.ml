type level = Pawn | Knight | Bishop | Rook | Queen | King
type color = White | Black
type position = (char, int)

type piece {
  position : position
  color : color
  level : level
  unused : bool
 }

let get_level p = p.level
let get_color p = p.color
let get_position = p.position

