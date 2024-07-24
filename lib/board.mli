type entry
(** What is stored at a position on the board, possibly None. *)

type t
(** The type of the board. *)

val initial : t
(** [initial] is a board state at the start of the game. *)

val empty : t
(** [empty] is the board with no pieces on it. *)

val add_piece : Piece.t -> string -> t -> t
(** [add_piece piece pos board] is [board] with [piece] added at position [pos]. *)

val print : t -> int -> ANSITerminal.style -> ANSITerminal.style -> unit
(** [print board color1 color2] prints the current board state with the given
    background colors. *)

val move_piece : string -> string -> t -> t
(** [move_piece x1 x2 board] is [board] with the piece at [x1] moved to [x2]. *)

val get_piece : string -> t -> Piece.t option
(** [get_piece p board] gets the chess piece at position [p] of the [board]. *)

val legal_position : string -> bool
(** [legal_position p] is whether [p] represents a legal position on the chess
    board. *)

val check_own : string -> string -> t -> bool
(** [check_own p_old p_new board] is whether [p_new] contains a piece of the
    same color as [p_old] on [board]. Requires: [p_old] and [p_new] are legal
    positions and [p_old] contain a piece. *)

val get_score : t -> Piece.color -> int
(** [get_score board c] returns the current score of the player for color [c]. *)

val check_promote_pawn : string -> string -> t -> bool
(** [check_promote_pawn pos_old pos_new board] is whether the piece at [pos_old]
    is a pawn, and the [pos_new] is at the edge of the board. Requires: the
    movement is a legal move. *)

val promote_pawn : string -> string -> string -> t -> t
(** [promote_pawn pos_old pos_new new_piece board] returns a new board with the
    pawn promoted to the [new_piece]. *)

val legal_bishop_move : string -> string -> t -> bool
(** [legal_bishop_move o n b] checks if moving a bishop from old position [o] to
    new position [n] on board [b] is a legal move. *)

val legal_queen_move : string -> string -> t -> bool
(** [legal_queen_move pos_old pos_new board] is whether a queen can move from
    [pos_old] to [pos_new] on [board]. *)

val legal_knight_move : string -> string -> bool
(** [legal_knight_move pos_old pos_new] is whether a knight can move from
    [pos_old] to [pos_new]. *)

val legal_pawn_move : string -> string -> t -> bool
(** [legal_pawn_move pos_old pos_new board] checks if moving a pawn from
    [pos_old] to [pos_new] on the [board] is legal or not. *)

val legal_king_move : string -> string -> bool
(** [legal_king_move pos_old pos_new] is whether a king can move from [pos_old]
    to [pos_new]. *)

val legal_rook_move : string -> string -> t -> bool
(** [legal_rook_move pos_old pos_new board] is whether a rook can move from
    [pos_old] to [pos_new] on [board]. *)

val legal_move : string -> string -> t -> bool
(** [legal_move pos_old pos_new board] is whether a player can move from
    [pos_old] to [pos_new] on [board]. *)

val legal_moves_list : string -> t -> string list
(** [legal_moves_list pos board] creates a list of all possible legal moves from
    current position [pos] on [board]. *)

val print_legal_moves : string list -> unit
(** [print_legal_moves list] prints the string representation [list] of legal
    moves if the user inputs an illegal move. *)

val check_castle : string -> string -> t -> bool
(** [check_castle pos_old pos_new board] is whether the piece is a king. If it
    is, it checkes if it can move from [pos_old] to [pos_new] through castling
    on [board]. *)

val castle : string -> string -> t -> t
(** [castle pos_old pos_new board] returns a new board with the king castled to
    its given position *)

val king_is_safe : t -> Piece.color -> bool
(** [king_is_safe board color] checks if the king of [color] on the [board] is
    safe or will be attacked. *)

val king_in_checkmate : t -> Piece.color -> bool
(** [king_in_checkmate board color] is whether the king of color [color] on the
    [board] is in checkmate. *)
