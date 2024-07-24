type name =
  | Queen
  | King
  | Knight
  | Rook
  | Bishop
  | Pawn  (** Represents type of the piece. *)

type color =
  | Black
  | White  (** Represents color of the piece. *)

type t
(** Represents a piece on the chess board. *)

val create : color -> name -> t
(** [create_piece n c] is a new piece of color [c] and name [n]. *)

val get_color : t -> color
(** [get_color p] is the color of [p]. *)

val get_piece : t -> name
(** [get_piece p] is the name of [p]. *)

val to_string : t -> string
(** [to_string p] is a unicode representation of [p]. *)

val name_string : name -> string
(** [name_string name] is a string representation of [name]. *)
