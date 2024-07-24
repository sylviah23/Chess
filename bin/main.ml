(* @authors: Sylvia Han (sjh326), Nancy Zheng (njz9), Jeanie Chan (jc2885),
   Anjelica Bian (yb265), Owen Chen (ojc7) *)

open Chess

let initial_board = Board.initial
let black_win = ref false
let white_win = ref false

let rec check_promotion old_pos new_pos board =
  let () = print_string "Promote to (Queen/Bishop/Rook/Knight) > " in
  let input = read_line () in
  match input with
  | s when s = "Queen" || s = "Bishop" || s = "Rook" || s = "Knight" ->
      Board.promote_pawn old_pos new_pos s board
  | _ -> check_promotion old_pos new_pos board

let color_of pos board =
  match Board.get_piece pos board with
  | Some p -> if Piece.get_color p = White then 0 else 1
  | _ -> failwith "empty pos"

let inspect_checkmate board turn =
  match turn with
  | 0 -> if Board.king_in_checkmate board Black then white_win := true else ()
  | 1 -> if Board.king_in_checkmate board White then black_win := true else ()
  | _ -> failwith "invalid turn argument"

let inspect_check board turn =
  (* if White just went, check if Black in check *)
  match turn with
  | 0 ->
      if not (Board.king_is_safe board Black) then
        print_endline "Black is in check!"
      else ()
  | 1 ->
      if not (Board.king_is_safe board White) then
        print_endline "White is in check!"
      else ()
  | _ -> failwith "invalid turn argument"

let print_turn turn =
  if turn = 0 then print_endline "\nWhite's turn"
  else print_endline "\nBlack's turn"

let safe_move board old_pos new_pos turn =
  let new_board = Board.move_piece old_pos new_pos board in
  match turn with
  | 0 -> Board.king_is_safe new_board White
  | 1 -> Board.king_is_safe new_board Black
  | _ -> failwith "invalid turn argument"

let get_move () =
  print_string "Move from > ";
  let old_pos = read_line () in
  print_string "Move to > ";
  let new_pos = read_line () in
  (old_pos, new_pos)

let redo_move old_pos board color1 color2 turn =
  if
    Board.legal_position old_pos
    && Board.get_piece old_pos board != None
    && turn = color_of old_pos board
  then (
    let piece_type =
      Piece.get_piece (Option.get (Board.get_piece old_pos board))
    in
    Printf.printf "Illegal move. Legal moves for %s at %s:\n"
      (Piece.name_string piece_type)
      old_pos;
    let legal_list = Board.legal_moves_list old_pos board in
    Board.print_legal_moves legal_list)
  else print_endline "Illegal move! Try again";
  Board.print board 0 color1 color2

let legal_castle old_pos new_pos board turn =
  Board.legal_position old_pos (* legal old position *)
  && Board.legal_position new_pos (* legal new position *)
  && Board.check_castle old_pos new_pos board (* ok castle *)
  && turn = color_of old_pos board (* piece is correct color *)

let legal_reg_move old_pos new_pos board turn =
  Board.legal_move old_pos new_pos board
  && turn = color_of old_pos board
  && safe_move board old_pos new_pos turn

let get_new_board old_pos new_pos board =
  if Board.check_promote_pawn old_pos new_pos board then
    check_promotion old_pos new_pos board
  else Board.move_piece old_pos new_pos board

let rec start_game board turn color1 color2 =
  print_turn turn;
  let old_pos, new_pos = get_move () in
  if legal_castle old_pos new_pos board turn then (
    let new_board = Board.castle old_pos new_pos board in
    Board.print new_board 0 color1 color2;
    start_game new_board ((turn + 1) mod 2) color1 color2)
  else if not (legal_reg_move old_pos new_pos board turn) then (
    redo_move old_pos board color1 color2 turn;
    start_game board turn color1 color2)
  else
    let new_board = get_new_board old_pos new_pos board in
    Board.print new_board 0 color1 color2;
    inspect_checkmate new_board turn;
    if !white_win then print_endline "\nCheckmate! White wins!"
    else if !black_win then print_endline "\nCheckmate! Black wins!"
    else (
      inspect_check new_board turn;
      start_game new_board ((turn + 1) mod 2) color1 color2)

let color_match = function
  | "red" -> ANSITerminal.on_red
  | "green" -> ANSITerminal.on_green
  | "yellow" -> ANSITerminal.on_yellow
  | "blue" -> ANSITerminal.on_blue
  | "magenta" -> ANSITerminal.on_magenta
  | "cyan" -> ANSITerminal.on_cyan
  | _ -> ANSITerminal.on_default

let help () =
  print_endline
    "Welcome to J.A.S.O.N's favorite game... chess!\n\
     Rules of the game:\n\
     Our program follows the usual rules for a game of chess. There are six \
     types of pieces: king, queen, rook, bishop, knight, and pawn. Here are \
     the rules corresponding to how they move (source: chess.com): \n\
    \    King - Moves one square in any direction.\n\
    \    Queen - Moves any number of squares diagonally, horizontally, or \
     vertically.\n\
    \    Rook - Moves any number of squares horizontally or vertically.\n\
    \    Bishop - Moves any number of squares diagonally.\n\
    \    Knight - Moves in an ‘L-shape,’ two squares in a straight direction, \
     and then one square perpendicular to that.\n\
    \    Pawn - Moves one square forward, but on its first move, it can move \
     two squares forward. It captures diagonally one square forward. \n\
     To make a move, simply enter your the current position of the piece you \
     want to move when the program asks where to move from. Then, enter the \
     position on the board you want to move to. If your move is legal, the \
     board will update, and it will be the other player's turn. If you make an \
     invalid move, our program will prompt you to take your turn again, \
     indicating that the move you entered is invalid. Please make sure to type \
     ALL CAPS when entering the positions on the board (i.e. type A3 instead \
     of a3). "

let rec start_game_custom_color c1 c2 ready_to_start =
  if ready_to_start then
    let () = Board.print initial_board 0 c1 c2 in
    start_game initial_board 0 c1 c2
  else
    let () =
      print_endline
        "Color(s) not supported. Please choose red, green, yellow, blue, \
         magenta, or cyan."
    in
    let () = print_string "Color 1:" in
    let c1 = color_match (read_line ()) in
    let () = print_string "Color 2:" in
    let c2 = color_match (read_line ()) in
    start_game_custom_color c1 c2
      (c1 <> ANSITerminal.on_default && c2 <> ANSITerminal.on_default)

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "-h" then help ()
  else if Array.length Sys.argv = 3 then
    let color1 = color_match Sys.argv.(1) in
    let color2 = color_match Sys.argv.(2) in
    start_game_custom_color color1 color2
      (color1 <> ANSITerminal.on_default && color2 <> ANSITerminal.on_default)
  else if Array.length Sys.argv <> 1 then
    print_endline
      "Usage: \n\
       to play the game with default board colors (red/green): dune exec \
       bin/main.exe\n\
       to play the game with custom board colors (options: red, green, yellow, \
       blue, magenta, cyan): dune exec bin/main.exe color1 color2\n\
       to see the rules of the game: dune exec bin/main.exe -- -h"
  else
    let () =
      Board.print initial_board 0 ANSITerminal.on_green ANSITerminal.on_red
    in
    start_game initial_board 0 ANSITerminal.on_green ANSITerminal.on_red
