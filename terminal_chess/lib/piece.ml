 type level = Pawn | Knight | Bishop | Rook | Queen | King
type color = White | Black
type position = (char * int) option

type piece = {
  position : position;
  color : color;
  level : level;
  rep : string;
 }

let get_level p = p.level
let get_color p = p.color

let get_position p = p.position

let place_piece pos c l rep = {
  position = pos;
  color = c;
  level = l;
  rep = rep;
 } 
  
let move_piece p pos =
  place_piece pos p.color p.level p.rep



