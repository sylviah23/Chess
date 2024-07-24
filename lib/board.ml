type entry = string * Piece.t option
type t = entry list list

type castle = {
  mutable king : bool;
  mutable rookA : bool;
  mutable rookH : bool;
}

let black_castle = { king = true; rookA = true; rookH = true }
let white_castle = { king = true; rookA = true; rookH = true }

let initial : t =
  let open Piece in
  [
    [
      ("A8", Some (create Black Rook));
      ("B8", Some (create Black Knight));
      ("C8", Some (create Black Bishop));
      ("D8", Some (create Black Queen));
      ("E8", Some (create Black King));
      ("F8", Some (create Black Bishop));
      ("G8", Some (create Black Knight));
      ("H8", Some (create Black Rook));
    ];
    [
      ("A7", Some (create Black Pawn));
      ("B7", Some (create Black Pawn));
      ("C7", Some (create Black Pawn));
      ("D7", Some (create Black Pawn));
      ("E7", Some (create Black Pawn));
      ("F7", Some (create Black Pawn));
      ("G7", Some (create Black Pawn));
      ("H7", Some (create Black Pawn));
    ];
    [
      ("A6", None);
      ("B6", None);
      ("C6", None);
      ("D6", None);
      ("E6", None);
      ("F6", None);
      ("G6", None);
      ("H6", None);
    ];
    [
      ("A5", None);
      ("B5", None);
      ("C5", None);
      ("D5", None);
      ("E5", None);
      ("F5", None);
      ("G5", None);
      ("H5", None);
    ];
    [
      ("A4", None);
      ("B4", None);
      ("C4", None);
      ("D4", None);
      ("E4", None);
      ("F4", None);
      ("G4", None);
      ("H4", None);
    ];
    [
      ("A3", None);
      ("B3", None);
      ("C3", None);
      ("D3", None);
      ("E3", None);
      ("F3", None);
      ("G3", None);
      ("H3", None);
    ];
    [
      ("A2", Some (create White Pawn));
      ("B2", Some (create White Pawn));
      ("C2", Some (create White Pawn));
      ("D2", Some (create White Pawn));
      ("E2", Some (create White Pawn));
      ("F2", Some (create White Pawn));
      ("G2", Some (create White Pawn));
      ("H2", Some (create White Pawn));
    ];
    [
      ("A1", Some (create White Rook));
      ("B1", Some (create White Knight));
      ("C1", Some (create White Bishop));
      ("D1", Some (create White Queen));
      ("E1", Some (create White King));
      ("F1", Some (create White Bishop));
      ("G1", Some (create White Knight));
      ("H1", Some (create White Rook));
    ];
  ]

let empty : t =
  [
    [
      ("A8", None);
      ("B8", None);
      ("C8", None);
      ("D8", None);
      ("E8", None);
      ("F8", None);
      ("G8", None);
      ("H8", None);
    ];
    [
      ("A7", None);
      ("B7", None);
      ("C7", None);
      ("D7", None);
      ("E7", None);
      ("F7", None);
      ("G7", None);
      ("H7", None);
    ];
    [
      ("A6", None);
      ("B6", None);
      ("C6", None);
      ("D6", None);
      ("E6", None);
      ("F6", None);
      ("G6", None);
      ("H6", None);
    ];
    [
      ("A5", None);
      ("B5", None);
      ("C5", None);
      ("D5", None);
      ("E5", None);
      ("F5", None);
      ("G5", None);
      ("H5", None);
    ];
    [
      ("A4", None);
      ("B4", None);
      ("C4", None);
      ("D4", None);
      ("E4", None);
      ("F4", None);
      ("G4", None);
      ("H4", None);
    ];
    [
      ("A3", None);
      ("B3", None);
      ("C3", None);
      ("D3", None);
      ("E3", None);
      ("F3", None);
      ("G3", None);
      ("H3", None);
    ];
    [
      ("A2", None);
      ("B2", None);
      ("C2", None);
      ("D2", None);
      ("E2", None);
      ("F2", None);
      ("G2", None);
      ("H2", None);
    ];
    [
      ("A1", None);
      ("B1", None);
      ("C1", None);
      ("D1", None);
      ("E1", None);
      ("F1", None);
      ("G1", None);
      ("H1", None);
    ];
  ]

let letter_to_number = function
  | 'A' -> 0
  | 'B' -> 1
  | 'C' -> 2
  | 'D' -> 3
  | 'E' -> 4
  | 'F' -> 5
  | 'G' -> 6
  | 'H' -> 7
  | _ -> failwith "out of bounds"

let get_piece_idx letter number board =
  snd (List.nth (List.nth board (8 - number)) letter)
(* Same as [get_piece] but positions use num *)

let get_piece position board =
  let letter = letter_to_number (String.get position 0) in
  let number = int_of_string (String.make 1 (String.get position 1)) in
  get_piece_idx letter number board
(* snd (List.nth (List.nth board (8 - number)) letter) *)

let rec update_row pos_old pos_new piece_moved row =
  match row with
  | [] -> []
  | h :: t ->
      if fst h = pos_old then
        (pos_old, None) :: update_row pos_old pos_new piece_moved t
      else if fst h = pos_new then
        (pos_new, piece_moved) :: update_row pos_old pos_new piece_moved t
      else h :: update_row pos_old pos_new piece_moved t

let update_board pos_old pos_new piece_moved board =
  ignore
    (match piece_moved with
    | None -> ()
    | Some p -> (
        match Piece.(get_piece p, get_color p) with
        | King, Black -> black_castle.king <- false
        | King, White -> white_castle.king <- false
        | Rook, Black when pos_old = "H1" -> black_castle.rookH <- false
        | Rook, Black when pos_old = "A1" -> black_castle.rookA <- false
        | Rook, White when pos_old = "H8" -> white_castle.rookH <- false
        | Rook, White when pos_old = "A8" -> white_castle.rookA <- false
        | _, _ -> ()));
  let rec helper pos_old pos_new piece_moved board =
    match board with
    | [] -> []
    | h :: t ->
        update_row pos_old pos_new piece_moved h
        :: helper pos_old pos_new piece_moved t
  in
  helper pos_old pos_new piece_moved board

let rec add_piece_to_row pos piece row =
  match row with
  | [] -> []
  | h :: t ->
      if fst h = pos then (pos, Some piece) :: t
      else h :: add_piece_to_row pos piece t

let rec add_piece_to_board pos piece board =
  match board with
  | [] -> []
  | h :: t -> add_piece_to_row pos piece h :: add_piece_to_board pos piece t

let add_piece (piece : Piece.t) (pos : string) (board : t) =
  add_piece_to_board pos piece board

let letters = [ "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H" ]
let numbers = [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8" ]

let score_counter board =
  let score_black = ref 39 in
  let score_white = ref 39 in
  for i = 0 to 7 do
    for j = 0 to 7 do
      let current = get_piece (List.nth letters i ^ List.nth numbers j) board in
      if current <> None then
        match
          ( Piece.get_color (Option.get current),
            Piece.get_piece (Option.get current) )
        with
        | Black, Pawn -> score_white := !score_white - 1
        | Black, Queen -> score_white := !score_white - 9
        | Black, Bishop | White, Knight -> score_white := !score_white - 3
        | Black, Rook -> score_white := !score_white - 5
        | White, Pawn -> score_black := !score_black - 1
        | White, Queen -> score_black := !score_black - 9
        | White, Bishop | Black, Knight -> score_black := !score_black - 3
        | White, Rook -> score_black := !score_black - 5
        | _ ->
            score_white := !score_white;
            score_black := !score_black
    done
  done;
  (!score_black, !score_white)

let get_score board = function
  | Piece.Black -> fst (score_counter board)
  | Piece.White -> snd (score_counter board)

let move_piece pos_old pos_new board =
  let piece_moved = get_piece pos_old board in
  update_board pos_old pos_new piece_moved board

let is_adjacent_letter letter1 letter2 =
  match letter1 with
  | 'A' -> letter2 = 'B'
  | 'B' -> letter2 = 'A' || letter2 = 'C'
  | 'C' -> letter2 = 'B' || letter2 = 'D'
  | 'D' -> letter2 = 'C' || letter2 = 'E'
  | 'E' -> letter2 = 'D' || letter2 = 'F'
  | 'F' -> letter2 = 'E' || letter2 = 'G'
  | 'G' -> letter2 = 'F' || letter2 = 'H'
  | 'H' -> letter2 = 'G'
  | _ -> failwith "letter not on board"

let legal_pawn_move pos_old pos_new board =
  let my_piece = get_piece pos_old board in
  let dest = get_piece pos_new board in
  let old_num = int_of_string (String.make 1 (String.get pos_old 1)) in
  let new_num = int_of_string (String.make 1 (String.get pos_new 1)) in
  let old_letter = String.get pos_old 0 in
  let new_letter = String.get pos_new 0 in
  if Piece.get_color (Option.get my_piece) = White then
    if new_num <= old_num || new_num > old_num + 2 then false
    else if new_num = old_num + 2 then
      old_num = 2 && old_letter = new_letter && dest = None
    else if old_letter = new_letter then dest = None
    else if is_adjacent_letter old_letter new_letter then dest <> None
    else false
  else if new_num >= old_num || new_num < old_num - 2 then false
  else if new_num = old_num - 2 then
    old_num = 7 && old_letter = new_letter && dest = None
  else if old_letter = new_letter then dest = None
  else if is_adjacent_letter old_letter new_letter then dest <> None
  else false

let rec print_row row i color1 color2 =
  match row with
  | [] -> print_string ""
  | (_, Some entry) :: rest ->
      let _ =
        if i mod 2 = 0 then
          ANSITerminal.print_string [ color1 ] (Piece.to_string entry ^ " ")
        else ANSITerminal.print_string [ color2 ] (Piece.to_string entry ^ " ")
      in
      print_row rest (i + 1) color1 color2
  | (_, None) :: rest ->
      let _ =
        if i mod 2 = 0 then ANSITerminal.print_string [ color1 ] "  "
        else ANSITerminal.print_string [ color2 ] "  "
      in
      print_row rest (i + 1) color1 color2

let rec print_helper board i color1 color2 =
  let row_i = string_of_int (8 - i) in
  match board with
  | [] -> print_endline "   A B C D E F G H"
  | r1 :: rest ->
      let _ = print_string (row_i ^ " ") in
      let _ = print_row r1 i color1 color2 in
      let _ = print_endline "" in
      print_helper rest (i + 1) color1 color2

let print board i color1 color2 =
  print_endline ("Score: " ^ string_of_int (get_score board Piece.Black));
  print_helper board i color1 color2;
  print_endline ("Score: " ^ string_of_int (get_score board Piece.White))

let number_to_letter = function
  | 0 -> 'A'
  | 1 -> 'B'
  | 2 -> 'C'
  | 3 -> 'D'
  | 4 -> 'E'
  | 5 -> 'F'
  | 6 -> 'G'
  | 7 -> 'H'
  | _ -> failwith "out of bounds"

let rec no_pieces_between letter_old number_old letter_new number_new board orig
    =
  if letter_old = letter_new && number_old = number_new then true
  else if
    get_piece
      (String.make 1 (number_to_letter letter_old) ^ string_of_int number_old)
      board
    <> None
    && orig = 1
  then false
  else if letter_new > letter_old && number_new > number_old then
    (* bishop is moving up right diagonally*)
    no_pieces_between (letter_old + 1) (number_old + 1) letter_new number_new
      board 1
  else if letter_new < letter_old && number_new < number_old then
    (* bishop is moving down left diagonally*)
    no_pieces_between (letter_old - 1) (number_old - 1) letter_new number_new
      board 1
  else if letter_new < letter_old && number_new > number_old then
    (* bishop is moving up left diagonally*)
    no_pieces_between (letter_old - 1) (number_old + 1) letter_new number_new
      board 1
  else
    (* bishop is moving down right diagonally*)
    no_pieces_between (letter_old + 1) (number_old - 1) letter_new number_new
      board 1

let legal_bishop_move pos_old pos_new (board : t) =
  let letter_old = letter_to_number (String.get pos_old 0) in
  let number_old = int_of_string (String.make 1 (String.get pos_old 1)) in
  let letter_new = letter_to_number (String.get pos_new 0) in
  let number_new = int_of_string (String.make 1 (String.get pos_new 1)) in
  (letter_old - letter_new = number_old - number_new
  || letter_old - letter_new = (number_old - number_new) * -1)
  && no_pieces_between letter_old number_old letter_new number_new board 0

let check_hv_higher is_h const var var' board =
  (* Higher order function for checking if there are no piece in between.
     Requires: is_h is true if checking pieces horizontally, false if check
     pieces vertically. Assumes that the piece is not moving to a location that
     contains its own piece. *)
  let start = min var var' in
  let stop = max var var' in

  let rec helper curr stop =
    if curr >= stop then true
    else
      match
        if is_h then get_piece_idx curr const board
        else get_piece_idx const curr board
      with
      | Some _ -> false
      | None -> helper (curr + 1) stop
  in
  helper (start + 1) stop

let check_hor num lett lett' board =
  (* Return true if there are no pieces in between *)
  check_hv_higher true num lett lett' board

let check_ver lett num num' board =
  (* Return true if there are no pieces in between *)
  check_hv_higher false lett num num' board

let check_dir lett num lett' num' =
  (* Returns true if the direction of movement is valid *)
  (lett = lett' || num = num') && not (lett = lett' && num = num')

(* Check if pos_new is directly up/down or side *)
(* Check if there are pieces between the route *)
(* Check if the new position contains a piece of its own color *)
let legal_rook_move pos_old pos_new board =
  let lett = letter_to_number (String.get pos_old 0) in
  let num = int_of_string (String.make 1 (String.get pos_old 1)) in

  let lett' = letter_to_number (String.get pos_new 0) in
  let num' = int_of_string (String.make 1 (String.get pos_new 1)) in

  let check_between lett num lett' num' =
    (* Returns false if there are pieces between the route *)
    if lett = lett' then check_ver lett num num' board
    else if num = num' then check_hor num lett lett' board
    else false
  in

  let result =
    check_dir lett num lett' num' && check_between lett num lett' num'
  in
  result

let check_own p_old p_new (board : t) =
  let p1 =
    match get_piece p_old board with
    | Some p -> p
    | None -> failwith "empty pos"
  in
  match get_piece p_new board with
  | Some p -> Piece.get_color p1 = Piece.get_color p
  | None -> false

let legal_position p =
  String.length p = 2
  && (let c = String.get p 0 in
      Char.code c >= Char.code 'A' && Char.code c <= Char.code 'H')
  &&
  let c = String.get p 1 in
  Char.code c >= Char.code '1' && Char.code c <= Char.code '8'

let legal_knight_move pos_old pos_new =
  if (not (legal_position pos_old)) || not (legal_position pos_new) then false
  else
    (* pos_old *)
    let l_old = 1 + letter_to_number (String.get pos_old 0) in
    let n_old = int_of_string (String.make 1 (String.get pos_old 1)) in

    (* pos_new *)
    let l_new = 1 + letter_to_number (String.get pos_new 0) in
    let n_new = int_of_string (String.make 1 (String.get pos_new 1)) in

    (* compute legal formulas *)
    (abs (l_old - l_new) = 1 && abs (n_old - n_new) = 2)
    || (abs (l_old - l_new) = 2 && abs (n_old - n_new) = 1)

let legal_queen_move pos_old pos_new board =
  legal_bishop_move pos_old pos_new board
  || legal_rook_move pos_old pos_new board

let legal_king_move pos_old pos_new =
  if (not (legal_position pos_old)) || not (legal_position pos_new) then false
  else
    (* pos_old *)
    let l_old = 1 + letter_to_number (String.get pos_old 0) in
    let n_old = int_of_string (String.make 1 (String.get pos_old 1)) in

    (* pos_new *)
    let l_new = 1 + letter_to_number (String.get pos_new 0) in
    let n_new = int_of_string (String.make 1 (String.get pos_new 1)) in

    (* compute legal formulas *)
    (abs (l_old - l_new) = 1 && abs (n_old - n_new) = 1)
    || (abs (l_old - l_new) = 0 && abs (n_old - n_new) = 1)
    || (abs (l_old - l_new) = 1 && abs (n_old - n_new) = 0)

let row8 = [ "A8"; "B8"; "C8"; "D8"; "E8"; "F8"; "G8"; "H8" ]
let row7 = [ "A7"; "B7"; "C7"; "D7"; "E7"; "F7"; "G7"; "H7" ]
let row6 = [ "A6"; "B6"; "C6"; "D6"; "E6"; "F6"; "G6"; "H6" ]
let row5 = [ "A5"; "B5"; "C5"; "D5"; "E5"; "F5"; "G5"; "H5" ]
let row4 = [ "A4"; "B4"; "C4"; "D4"; "E4"; "F4"; "G4"; "H4" ]
let row3 = [ "A3"; "B3"; "C3"; "D3"; "E3"; "F3"; "G3"; "H3" ]
let row2 = [ "A2"; "B2"; "C2"; "D2"; "E2"; "F2"; "G2"; "H2" ]
let row1 = [ "A1"; "B1"; "C1"; "D1"; "E1"; "F1"; "G1"; "H1" ]

let all_positions =
  List.flatten [ row8; row7; row6; row5; row4; row3; row2; row1 ]

let rec king_pos_helper board color idx =
  let pos = List.nth all_positions idx in
  let piece = get_piece pos board in
  if
    piece <> None
    && Piece.get_color (Option.get piece) = color
    && Piece.get_piece (Option.get piece) = Piece.King
  then pos
  else king_pos_helper board color (idx + 1)

let king_pos board color = king_pos_helper board color 0

let piece_can_capture_king piece king_color pos board =
  match Piece.get_piece piece with
  | Queen ->
      if king_color = Piece.Black then
        legal_queen_move pos (king_pos board Piece.Black) board
      else legal_queen_move pos (king_pos board Piece.White) board
  | King ->
      if king_color = Piece.Black then
        legal_king_move pos (king_pos board Piece.Black)
      else legal_king_move pos (king_pos board Piece.White)
  | Knight ->
      if king_color = Piece.Black then
        legal_knight_move pos (king_pos board Piece.Black)
      else legal_knight_move pos (king_pos board Piece.White)
  | Rook ->
      if king_color = Piece.Black then
        legal_rook_move pos (king_pos board Piece.Black) board
      else legal_rook_move pos (king_pos board Piece.White) board
  | Bishop ->
      if king_color = Piece.Black then
        legal_bishop_move pos (king_pos board Piece.Black) board
      else legal_bishop_move pos (king_pos board Piece.White) board
  | Pawn ->
      if king_color = Piece.Black then
        legal_pawn_move pos (king_pos board Piece.Black) board
      else legal_pawn_move pos (king_pos board Piece.White) board

let rec king_is_safe_helper (board : t) color pos_idx acc =
  if acc = false then false
  else if pos_idx >= List.length all_positions then acc
  else
    let pos = List.nth all_positions pos_idx in

    let piece = get_piece pos board in
    if piece <> None && Piece.get_color (Option.get piece) <> color then
      king_is_safe_helper board color (pos_idx + 1)
        ((not (piece_can_capture_king (Option.get piece) color pos board))
        && acc)
    else king_is_safe_helper board color (pos_idx + 1) acc

let king_is_safe board color = king_is_safe_helper board color 0 true

let check_promote_pawn pos_old pos_new board =
  let piece =
    match get_piece pos_old board with
    | Some p -> p
    | None -> failwith "empty pos"
  in
  match Piece.(get_color piece, get_piece piece) with
  | Piece.White, Piece.Pawn when String.get pos_new 1 = '8' -> true
  | Piece.Black, Piece.Pawn when String.get pos_new 1 = '1' -> true
  | _, _ -> false

let promote_pawn pos_old pos_new new_piece board =
  let piece =
    match get_piece pos_old board with
    | Some p -> p
    | None -> failwith "empty pos"
  in
  let color = Piece.get_color piece in
  let np =
    match new_piece with
    | "Queen" -> Some Piece.(create color Queen)
    | "Bishop" -> Some Piece.(create color Bishop)
    | "Rook" -> Some Piece.(create color Rook)
    | "Knight" -> Some Piece.(create color Knight)
    | _ -> failwith "invalid piece"
  in
  update_board pos_old pos_new np board

let legal_move p_old p_new board =
  if (not (legal_position p_old)) || not (legal_position p_new) then false
  else
    match get_piece p_old board with
    | None -> false
    | Some p -> (
        if check_own p_old p_new board then false
        else
          match Piece.get_piece p with
          | Queen -> legal_queen_move p_old p_new board
          | King -> legal_king_move p_old p_new
          | Knight -> legal_knight_move p_old p_new
          | Rook -> legal_rook_move p_old p_new board
          | Bishop -> legal_bishop_move p_old p_new board
          | Pawn -> legal_pawn_move p_old p_new board)

let legal_moves_list pos board =
  let all_positions =
    List.flatten (List.map (fun row -> List.map fst row) board)
  in
  List.filter
    (fun pos_new -> legal_position pos_new && legal_move pos pos_new board)
    all_positions

let print_legal_moves list =
  List.iter (fun pos_new -> Printf.printf "  %s\n" pos_new) list

let check_castle pos_old pos_new board =
  match get_piece pos_old board with
  | None -> false
  | Some p -> (
      let name = Piece.get_piece p in
      let color = Piece.get_color p in
      match (name, color, pos_new) with
      | King, Black, "G8" ->
          check_hor 8 4 7 board && black_castle.king && black_castle.rookH
      | King, Black, "C8" ->
          check_hor 8 0 4 board && black_castle.king && black_castle.rookA
      | King, White, "G1" ->
          check_hor 1 4 7 board && white_castle.king && white_castle.rookH
      | King, White, "C1" ->
          check_hor 1 0 4 board && white_castle.king && white_castle.rookA
      | _, _, _ -> false)

let castle pos_old pos_new board =
  let piece_moved = get_piece pos_old board in
  let temp_board = update_board pos_old pos_new piece_moved board in
  match pos_new with
  | "G8" ->
      let p = get_piece "H8" temp_board in
      update_board "H8" "F8" p temp_board
  | "C8" ->
      let p = get_piece "A8" temp_board in
      update_board "A8" "D8" p temp_board
  | "G1" ->
      let p = get_piece "H1" temp_board in
      update_board "H1" "F1" p temp_board
  | "C1" ->
      let p = get_piece "A1" temp_board in
      update_board "A1" "D1" p temp_board
  | _ -> board

(** [adj_pos pos] is all the adjacent positions to [pos] on the board. *)
let adj_pos pos =
  let lett = letter_to_number (String.get pos 0) in
  let num = int_of_string (String.make 1 (String.get pos 1)) in

  let combiner acc new_pos =
    let new_lett = letter_to_number (String.get new_pos 0) in
    let new_num = int_of_string (String.make 1 (String.get new_pos 1)) in
    if
      (new_lett = lett + 1 || new_lett = lett - 1 || new_lett = lett)
      && (new_num = num + 1 || new_num = num - 1 || new_num = num)
      && not (new_lett = lett && new_num = num)
    then new_pos :: acc
    else acc
  in

  List.fold_left combiner [] all_positions

let king_in_checkmate (board : t) color =
  let pos = king_pos board color in
  let possible_moves = adj_pos pos in

  let combiner acc new_pos =
    if legal_move pos new_pos board then
      acc || king_is_safe (move_piece pos new_pos board) color
    else acc
  in
  (* return true if and only if king is not safe in any of its possible moves *)
  not (king_is_safe board color || List.fold_left combiner false possible_moves)
