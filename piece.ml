type level = Pawn | Knight | Bishop | Rook | Queen | King
type color = White | Black
type position = (char, int)

type piece {
  position : position
  color : color
  level : level
  unused : bool
 }


