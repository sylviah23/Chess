type name =
  | Queen
  | King
  | Knight
  | Rook
  | Bishop
  | Pawn

type color =
  | Black
  | White

type t = {
  name : name;
  color : color;
}

let create c n = { name = n; color = c }
let get_color p = p.color
let get_piece p = p.name

let to_string p =
  match p with
  | { name = Queen; color = White } -> "\u{2655}"
  | { name = King; color = White } -> "\u{2654}"
  | { name = Knight; color = White } -> "\u{2658}"
  | { name = Rook; color = White } -> "\u{2656}"
  | { name = Bishop; color = White } -> "\u{2657}"
  | { name = Pawn; color = White } -> "\u{2659}"
  | { name = Queen; color = Black } -> "\u{265B}"
  | { name = King; color = Black } -> "\u{265A}"
  | { name = Knight; color = Black } -> "\u{265E}"
  | { name = Rook; color = Black } -> "\u{265C}"
  | { name = Bishop; color = Black } -> "\u{265D}"
  | { name = Pawn; color = Black } -> "\u{265F}"

let name_string name =
  match name with
  | Queen -> "Queen"
  | King -> "King"
  | Knight -> "Knight"
  | Rook -> "Rook"
  | Bishop -> "Bishop"
  | Pawn -> "Pawn"
