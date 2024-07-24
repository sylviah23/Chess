open OUnit2
open Chess

module type T = sig
  val tests : test
end

module CommonTests = struct
  (** A module of common tests *)

  (** [test_piece piece color name] tests the piece and color of a piece *)
  let test_piece piece color name =
    [
      ( "test get piece and get color" >:: fun _ ->
        assert_equal (Piece.get_color piece) color );
      ( "test get piece and get name" >:: fun _ ->
        assert_equal (Piece.get_piece piece) name );
    ]
end

(* BOARD *)

module CommonPieces_Board = struct
  let initial = Board.initial
  let black_rook = Option.get (Board.get_piece "A8" Board.initial)
  let black_pawn = Option.get (Board.get_piece "D7" Board.initial)
  let white_knight = Option.get (Board.get_piece "G1" Board.initial)
  let white_pawn = Option.get (Board.get_piece "B2" Board.initial)
end

module TestBoardInit : T = struct
  open CommonTests
  open CommonPieces_Board

  let tests =
    "Test inital board: get_piece & get_color"
    >::: List.flatten
           [
             test_piece black_rook Black Rook;
             test_piece black_pawn Black Pawn;
             test_piece white_knight White Knight;
             test_piece white_pawn White Pawn;
           ]
end

module TestLegalPos : T = struct
  let tests =
    "Test legal_position"
    >::: [
           ( "Test legal 1" >:: fun _ ->
             assert_equal (Board.legal_position "A3") true );
           ( "Test legal 2" >:: fun _ ->
             assert_equal (Board.legal_position "B8") true );
           ( "Test legal 3" >:: fun _ ->
             assert_equal (Board.legal_position "H1") true );
           ( "Test legal 4" >:: fun _ ->
             assert_equal (Board.legal_position "F7") true );
           ( "Test illegal 1" >:: fun _ ->
             assert_equal (Board.legal_position "A31") false );
           ( "Test illegal 2" >:: fun _ ->
             assert_equal (Board.legal_position "I1") false );
           ( "Test illegal 3" >:: fun _ ->
             assert_equal (Board.legal_position "AA3") false );
           ( "Test illegal 4" >:: fun _ ->
             assert_equal (Board.legal_position "F0") false );
         ]
end

module TestCreatePiece : T = struct
  let tests =
    "Test create piece with get color and get piece"
    >::: [
           ( "Test create black knight color" >:: fun _ ->
             assert_equal
               (Piece.get_color (Piece.create Piece.Black Piece.Knight))
               Piece.Black );
           ( "Test create black knight name" >:: fun _ ->
             assert_equal
               (Piece.get_piece (Piece.create Piece.Black Piece.Knight))
               Piece.Knight );
           ( "Test create white king color" >:: fun _ ->
             assert_equal
               (Piece.get_color (Piece.create Piece.White Piece.King))
               Piece.White );
           ( "Test create black king name" >:: fun _ ->
             assert_equal
               (Piece.get_piece (Piece.create Piece.Black Piece.King))
               Piece.King );
           ( "Test create black rook color" >:: fun _ ->
             assert_equal
               (Piece.get_color (Piece.create Piece.Black Piece.Rook))
               Piece.Black );
           ( "Test create white bishop name" >:: fun _ ->
             assert_equal
               (Piece.get_piece (Piece.create Piece.White Piece.Bishop))
               Piece.Bishop );
         ]
end

module TestMovePiece : T = struct
  open CommonPieces_Board

  let b1 = Board.move_piece "A2" "B5" initial
  let b2 = Board.move_piece "F1" "F2" b1
  let b3 = Board.move_piece "F2" "B5" b2
  let letters = [ "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H" ]
  let numbers = [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8" ]

  let same_pieces old_pos new_pos original updated =
    let checked = ref true in
    for i = 0 to 7 do
      for j = 0 to 7 do
        if
          old_pos <> List.nth letters i ^ List.nth numbers j
          && new_pos <> List.nth letters i ^ List.nth numbers j
        then
          if
            Board.get_piece (List.nth letters i ^ List.nth numbers j) original
            <> Board.get_piece (List.nth letters i ^ List.nth numbers j) updated
          then checked := false
      done
    done;
    !checked

  let tests =
    "Test move piece with get piece"
    >::: [
           ( "Moving a piece should not move any other pieces" >:: fun _ ->
             assert_equal true (same_pieces "A2" "B5" initial b1) );
           ( "Test orig position has nothing" >:: fun _ ->
             assert_equal (Board.get_piece "A2" b1) None );
           ( "Test new position has something" >:: fun _ ->
             assert_equal
               (Piece.get_piece (Option.get (Board.get_piece "B5" b1)))
               Piece.Pawn );
           ( "Test orig position from prev move still has nothing" >:: fun _ ->
             assert_equal (Board.get_piece "A2" b2 == None) true );
           ( "Test new position from last move still has same thing" >:: fun _ ->
             assert_equal
               (Piece.get_piece (Option.get (Board.get_piece "B5" b2)))
               Piece.Pawn );
           ( "Test orig position from curr move has nothing" >:: fun _ ->
             assert_equal (Board.get_piece "F1" b2 == None) true );
           ( "Test new position from last move still has same thing" >:: fun _ ->
             assert_equal
               (Piece.get_piece (Option.get (Board.get_piece "F2" b2)))
               Piece.Bishop );
           ( "Test that curr position has nothing" >:: fun _ ->
             assert_equal (Board.get_piece "F2" b3) None );
           ( "Test that new position has the same piece as last move"
           >:: fun _ ->
             assert_equal
               (Piece.get_piece (Option.get (Board.get_piece "B5" b3)))
               Piece.Bishop );
         ]
end

module TestGetPiece : T = struct
  open CommonPieces_Board

  let test_one name str piece =
    name >:: fun _ -> assert_equal (Board.get_piece str initial) (Some piece)

  let tests1 =
    [
      test_one "Test black_rook" "A8" black_rook;
      test_one "Test black_pawn" "D7" black_pawn;
      test_one "Test white_knight" "G1" white_knight;
      test_one "Test white_pawn" "B2" white_pawn;
    ]

  let tests2 =
    [
      ( "Test black knight color" >:: fun _ ->
        assert_equal
          (Piece.get_color (Option.get (Board.get_piece "G8" initial)))
          Piece.Black );
      ( "Test black knight name" >:: fun _ ->
        assert_equal
          (Piece.get_piece (Option.get (Board.get_piece "G8" initial)))
          Piece.Knight );
      ( "Test white rook name" >:: fun _ ->
        assert_equal
          (Piece.get_piece (Option.get (Board.get_piece "A1" initial)))
          Piece.Rook );
      ( "Test white rook color" >:: fun _ ->
        assert_equal
          (Piece.get_color (Option.get (Board.get_piece "H1" initial)))
          Piece.White );
      ( "Test black king color" >:: fun _ ->
        assert_equal
          (Piece.get_color (Option.get (Board.get_piece "E8" initial)))
          Piece.Black );
      ( "Test black king name" >:: fun _ ->
        assert_equal
          (Piece.get_piece (Option.get (Board.get_piece "E8" initial)))
          Piece.King );
      ( "Test white king color" >:: fun _ ->
        assert_equal
          (Piece.get_color (Option.get (Board.get_piece "E1" initial)))
          Piece.White );
    ]

  let tests3 =
    [
      ( "Test white king name" >:: fun _ ->
        assert_equal
          (Piece.get_piece (Option.get (Board.get_piece "E1" initial)))
          Piece.King );
      ( "Test black bishop color" >:: fun _ ->
        assert_equal
          (Piece.get_color (Option.get (Board.get_piece "C8" initial)))
          Piece.Black );
      ( "Test black bishop name" >:: fun _ ->
        assert_equal
          (Piece.get_piece (Option.get (Board.get_piece "F8" initial)))
          Piece.Bishop );
      ( "Test white bishop color" >:: fun _ ->
        assert_equal
          (Piece.get_color (Option.get (Board.get_piece "C1" initial)))
          Piece.White );
      ( "Test white bishop name" >:: fun _ ->
        assert_equal
          (Piece.get_piece (Option.get (Board.get_piece "F1" initial)))
          Piece.Bishop );
    ]

  let tests4 =
    [
      ( "Test black queen color" >:: fun _ ->
        assert_equal
          (Piece.get_color (Option.get (Board.get_piece "D8" initial)))
          Piece.Black );
      ( "Test black queen name" >:: fun _ ->
        assert_equal
          (Piece.get_piece (Option.get (Board.get_piece "D8" initial)))
          Piece.Queen );
      ( "Test white queen color" >:: fun _ ->
        assert_equal
          (Piece.get_color (Option.get (Board.get_piece "D1" initial)))
          Piece.White );
      ( "Test white queen name" >:: fun _ ->
        assert_equal
          (Piece.get_piece (Option.get (Board.get_piece "D1" initial)))
          Piece.Queen );
    ]

  let all_tests = List.flatten [ tests1; tests2; tests3; tests4 ]
  let tests = "Test inital board: get_piece" >::: all_tests
end

module TestScore : T = struct
  let b1 = Board.move_piece "D1" "C4" Board.initial
  let b2 = Board.move_piece "H1" "H7" b1
  let b3 = Board.move_piece "A8" "A2" b2
  let b4 = Board.move_piece "A1" "A2" b3
  let b5 = Board.move_piece "B2" "C4" b4

  let tests =
    "Tests score functionality"
    >::: [
           ( "scores starts at 0" >:: fun _ ->
             assert_equal
               ( Board.get_score Board.initial Piece.Black,
                 Board.get_score Board.initial Piece.White )
               (0, 0) );
           ( "no pieces eaten means score doesn't change" >:: fun _ ->
             assert_equal
               (Board.get_score b1 Piece.Black, Board.get_score b1 Piece.White)
               (0, 0) );
           ( "eating a black pawn increases white score by 1" >:: fun _ ->
             assert_equal
               (Board.get_score b2 Piece.Black, Board.get_score b2 Piece.White)
               (0, 1) );
           ( "eating a white pawn increases black score by 1" >:: fun _ ->
             assert_equal
               (Board.get_score b3 Piece.Black, Board.get_score b3 Piece.White)
               (1, 1) );
           ( "eating a black rook increases white score by 5" >:: fun _ ->
             assert_equal
               (Board.get_score b4 Piece.Black, Board.get_score b4 Piece.White)
               (1, 6) );
           ( "eating white queen increases black score by 9" >:: fun _ ->
             assert_equal
               (Board.get_score b5 Piece.Black, Board.get_score b5 Piece.White)
               (10, 6) );
         ]
end

module TestMoveKnight : T = struct
  let tests =
    "Test knight moves"
    >::: [
           ( "test check_knight" >:: fun _ ->
             assert (Board.legal_knight_move "D4" "B5") );
           ( "test check_knight" >:: fun _ ->
             assert (Board.legal_knight_move "D4" "C6") );
           ( "test check_knight" >:: fun _ ->
             assert (Board.legal_knight_move "D4" "F5") );
           ( "test check_knight" >:: fun _ ->
             assert (Board.legal_knight_move "D4" "E6") );
           ( "test check_knight" >:: fun _ ->
             assert (Board.legal_knight_move "D4" "B3") );
           ( "test check_knight" >:: fun _ ->
             assert (Board.legal_knight_move "D4" "C2") );
           ( "test check_knight" >:: fun _ ->
             assert (Board.legal_knight_move "D4" "E2") );
           ( "test check_knight" >:: fun _ ->
             assert (Board.legal_knight_move "D4" "F3") );
           ( "test check_knight" >:: fun _ ->
             assert_equal false (Board.legal_knight_move "C4" "B5") );
           ( "test check_knight" >:: fun _ ->
             assert_equal false (Board.legal_knight_move "C4" "C6") );
           ( "test check_knight" >:: fun _ ->
             assert_equal false (Board.legal_knight_move "C4" "F5") );
           ( "test check_knight" >:: fun _ ->
             assert_equal false (Board.legal_knight_move "C4" "E6") );
           ( "test check_knight" >:: fun _ ->
             assert_equal false (Board.legal_knight_move "C4" "B3") );
           ( "test check_knight" >:: fun _ ->
             assert_equal false (Board.legal_knight_move "C4" "C2") );
           ( "test check_knight" >:: fun _ ->
             assert_equal false (Board.legal_knight_move "C4" "E2") );
           ( "test check_knight" >:: fun _ ->
             assert_equal false (Board.legal_knight_move "C4" "F3") );
         ]
end

module TestMoveBishop : T = struct
  open CommonPieces_Board

  let moved_pawn = Board.move_piece "D2" "D3" initial
  let moved_bishop1 = Board.move_piece "C1" "G5" moved_pawn
  let moved_bishop2 = Board.move_piece "C8" "D5" initial
  let moved_bishop3 = Board.move_piece "C8" "E5" initial

  let tests =
    "Test legal moves of bishop"
    >::: [
           ( "moving bishop diagonally up right one square is legal" >:: fun _ ->
             assert_equal true (Board.legal_bishop_move "C1" "D2" moved_pawn) );
           ( "moving bishop diagonally multiple squares up right is legal"
           >:: fun _ ->
             assert_equal true (Board.legal_bishop_move "C1" "G5" moved_pawn) );
           ( "moving bishop diagonally multiple squares up left is legal"
           >:: fun _ ->
             assert_equal true (Board.legal_bishop_move "D5" "B7" moved_bishop2)
           );
           ( "moving bishop diag multiple squares down right is legal"
           >:: fun _ ->
             assert_equal true (Board.legal_bishop_move "D5" "F3" moved_bishop2)
           );
           ( "moving bishop diag multiple squares down left is legal"
           >:: fun _ ->
             assert_equal true (Board.legal_bishop_move "D5" "B3" moved_bishop2)
           );
           ( "moving bishop up right w/ pieces between is illegal" >:: fun _ ->
             assert_equal false
               (Board.legal_bishop_move "C1" "G5" moved_bishop2) );
           ( "moving bishop up left with pieces in between is not legal"
           >:: fun _ ->
             assert_equal false
               (Board.legal_bishop_move "D5" "A8" moved_bishop2) );
           ( "moving bishop down right with pieces in between is not legal"
           >:: fun _ ->
             assert_equal false
               (Board.legal_bishop_move "D5" "H1" moved_bishop2) );
           ( "moving bishop down left with pieces in between is not legal"
           >:: fun _ ->
             assert_equal false
               (Board.legal_bishop_move "E5" "A1" moved_bishop3) );
           ( "eating an enemy piece with a bishop is legal" >:: fun _ ->
             assert_equal true (Board.legal_bishop_move "G5" "E7" moved_bishop1)
           );
           ( "not moving diagonally is not legal" >:: fun _ ->
             assert_equal false
               (Board.legal_bishop_move "E5" "E6" moved_bishop3) );
         ]
end

module TestMoveRook : T = struct
  open Board

  let init = initial
  let b1 = move_piece "H2" "H4" init
  let b2 = move_piece "G2" "G4" init
  let b3 = move_piece "F2" "F4" b1
  let b4 = move_piece "A7" "A6" init
  let b5 = move_piece "G8" "G7" init
  let b6 = move_piece "G7" "F6" init

  let tests =
    "Test Move Rook Check"
    >::: [
           ( "test jump over piece vertically" >:: fun _ ->
             assert_equal (legal_rook_move "H1" "H3" init) false );
           ( "test valid move vertically" >:: fun _ ->
             assert_equal (legal_rook_move "H1" "H2" b1) true );
           ( "test jump over piece horizontally" >:: fun _ ->
             assert_equal
               (move_piece "H1" "H2" b3 |> legal_rook_move "H2" "F2")
               false );
           ( "test valid move horizontally" >:: fun _ ->
             assert_equal
               (move_piece "H1" "H3" b1 |> legal_rook_move "H3" "E3")
               true );
           ( "test move diagonally with no piece" >:: fun _ ->
             assert_equal (legal_rook_move "H1" "G2" b2) false );
           ( "test move diagonally with piece" >:: fun _ ->
             assert_equal (legal_rook_move "H1" "G2" init) false );
           ( "test move down with no piece" >:: fun _ ->
             assert_equal (legal_rook_move "A8" "A7" b4) true );
           ( "test move down with piece, capture" >:: fun _ ->
             assert_equal (legal_rook_move "A8" "A7" init) true );
           ( "test move left with no piece" >:: fun _ ->
             assert_equal (legal_rook_move "H8" "G8" b5) true );
           ( "test move left with piece, capture" >:: fun _ ->
             assert_equal (legal_rook_move "H8" "G8" init) true );
           ( "test move diag with no piece" >:: fun _ ->
             assert_equal (legal_rook_move "H8" "G7" b6) false );
           ( "test move diag with piece" >:: fun _ ->
             assert_equal (legal_rook_move "H8" "G7" init) false );
         ]
end

module TestMoveQueen : T = struct
  open Board

  let b1 = move_piece "D2" "D4" initial
  let b2 = move_piece "C2" "C4" b1
  let b3 = move_piece "B7" "B3" b2
  let b4 = initial |> move_piece "D1" "E4" |> move_piece "B7" "B4"
  let b5 = initial |> move_piece "E2" "E4" |> move_piece "G7" "G4"

  let tests =
    "Test move queen "
    >::: [
           ( "vert up with none inbetween" >:: fun _ ->
             assert_equal (legal_queen_move "D1" "D3" b1) true );
           ( "vert down with none inbetween" >:: fun _ ->
             assert_equal (legal_queen_move "D3" "D1" b1) true );
           ( "vert up with piece inbetween" >:: fun _ ->
             assert_equal (legal_queen_move "D1" "D5" b1) false );
           ( "vert down with piece inbetween" >:: fun _ ->
             assert_equal (legal_queen_move "D5" "D1" b1) false );
           ( "diag up left with none inbetween" >:: fun _ ->
             assert_equal (legal_queen_move "D1" "B3" b3) true );
           ( "diag down right with none inbetween" >:: fun _ ->
             assert_equal (legal_queen_move "B3" "D1" b3) true );
           ( "diag up left with piece inbetween" >:: fun _ ->
             assert_equal (legal_queen_move "D1" "A4" b3) false );
           ( "diag down right with piece inbetween" >:: fun _ ->
             assert_equal (legal_queen_move "A4" "D1" b3) false );
           ( "diag up right with none inbetween" >:: fun _ ->
             assert_equal (legal_queen_move "D1" "F3" b5) true );
           ( "diag down left with none inbetween" >:: fun _ ->
             assert_equal (legal_queen_move "F3" "D1" b5) true );
           ( "diag up right with piece inbetween" >:: fun _ ->
             assert_equal (legal_queen_move "D1" "H5" b5) false );
           ( "diag down left with piece inbetween" >:: fun _ ->
             assert_equal (legal_queen_move "H5" "D1" b5) false );
           ( "hor left with none inbetween" >:: fun _ ->
             assert_equal (legal_queen_move "E4" "C4" b4) true );
           ( "hor right with none inbetween" >:: fun _ ->
             assert_equal (legal_queen_move "C4" "E4" b4) true );
           ( "hor left with piece inbetween" >:: fun _ ->
             assert_equal (legal_queen_move "E4" "A4" b4) false );
           ( "hor right with piece inbetween" >:: fun _ ->
             assert_equal (legal_queen_move "A4" "E4" b4) false );
         ]
end

module TestMovePawn : T = struct
  let b = Board.move_piece "C2" "C4" Board.initial
  let b = Board.move_piece "D7" "D5" b
  let b2 = Board.move_piece "D2" "D3" Board.initial
  let b2 = Board.move_piece "D7" "D5" b2
  let b2 = Board.move_piece "D5" "D4" b2
  let b2 = Board.move_piece "C2" "C3" b2
  let b2 = Board.move_piece "E7" "E5" b2

  let tests_init1 =
    [
      ( "test valid move for init white pawn (up once)" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "B2" "B3" Board.initial) true );
      ( "test valid move for init white pawn (up twice)" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "C2" "C4" Board.initial) true );
      ( "test valid move for init black pawn (down once)" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "A7" "A6" Board.initial) true );
      ( "test valid move for init black pawn (down twice)" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "G7" "G5" Board.initial) true );
      ( "test not valid move for init black pawn (diagonal)" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "G7" "H6" Board.initial) false );
      ( "test not valid move for init black pawn (random)" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "G7" "A1" Board.initial) false );
      ( "test not valid move for init white pawn (same row)" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "D2" "E2" Board.initial) false );
    ]

  let tests_2 =
    [
      ( "test valid eat for white pawn" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "C4" "D5" b) true );
      ( "test valid eat for black pawn" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "D5" "C4" b) true );
      ( "test not valid horizontal move for white pawn" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "D3" "E3" b2) false );
      ( "test not valid random move for white pawn" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "D3" "F8" b2) false );
      ( "test legal eat for black pawn" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "D4" "C3" b2) true );
      ( "test not legal eat for black pawn" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "D4" "D3" b2) false );
    ]

  let tests_3 =
    [
      ( "test white up twice when not in starting position" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "C3" "C5" b2) false );
      ( "test black down twice when not in starting position" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "D4" "D2" b2) false );
      ( "test white go down" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "C3" "C2" b2) false );
      ( "test black go up" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "D4" "D5" b2) false );
      ( "test black go down once but something in the way" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "D4" "D3" b2) false );
      ( "test white go up once" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "C3" "C4" b2) true );
      ( "test black go down once" >:: fun _ ->
        assert_equal (Board.legal_pawn_move "E5" "E4" b2) true );
    ]

  let tests =
    "Test pawn moves" >::: List.flatten [ tests_init1; tests_2; tests_3 ]
end

module CommonPieces_Piece = struct
  let black_rook = Piece.create Black Rook
  let black_pawn = Piece.create Black Pawn
  let white_knight = Piece.create White Knight
  let white_pawn = Piece.create White Pawn
end

module TestPawnPromotion : T = struct
  open Board

  let b = Board.initial

  let test_white =
    [
      ( "test white Pawn to Queen from init" >:: fun _ ->
        assert_equal
          (Board.promote_pawn "A2" "A8" "Queen" b |> get_piece "A8")
          (Some Piece.(create White Queen)) );
      ( "test white Pawn to Bishop from init" >:: fun _ ->
        assert_equal
          (Board.promote_pawn "B2" "E8" "Bishop" b |> get_piece "E8")
          (Some Piece.(create White Bishop)) );
      ( "test white Pawn to Rook from init" >:: fun _ ->
        assert_equal
          (Board.promote_pawn "C2" "G8" "Rook" b |> get_piece "G8")
          (Some Piece.(create White Rook)) );
      ( "test white Pawn to Knight from init" >:: fun _ ->
        assert_equal
          (Board.promote_pawn "D2" "D8" "Knight" b |> get_piece "D8")
          (Some Piece.(create White Knight)) );
    ]

  let test_black =
    [
      ( "test black Pawn to Queen" >:: fun _ ->
        assert_equal
          (Board.promote_pawn "A7" "A1" "Queen" b |> get_piece "A1")
          (Some Piece.(create Black Queen)) );
      ( "test black Pawn to Bishop" >:: fun _ ->
        assert_equal
          (Board.promote_pawn "B7" "E1" "Bishop" b |> get_piece "E1")
          (Some Piece.(create Black Bishop)) );
      ( "test black Pawn to Rook" >:: fun _ ->
        assert_equal
          (Board.promote_pawn "C7" "G1" "Rook" b |> get_piece "G1")
          (Some Piece.(create Black Rook)) );
      ( "test black Pawn to Knight" >:: fun _ ->
        assert_equal
          (Board.promote_pawn "D7" "D1" "Knight" b |> get_piece "D1")
          (Some Piece.(create Black Knight)) );
    ]

  let b1 = Board.move_piece "B2" "B4" b
  let b2 = Board.move_piece "B2" "D4" b
  let b3 = Board.move_piece "B2" "C6" b
  let b4 = Board.move_piece "B2" "G7" b

  let test' =
    [
      ( "test white Pawn to Queen from init" >:: fun _ ->
        assert_equal
          (Board.promote_pawn "B4" "A8" "Queen" b1 |> get_piece "A8")
          (Some Piece.(create White Queen)) );
      ( "test white Pawn to Bishop from init" >:: fun _ ->
        assert_equal
          (Board.promote_pawn "D4" "E8" "Bishop" b2 |> get_piece "E8")
          (Some Piece.(create White Bishop)) );
      ( "test white Pawn to Rook from init" >:: fun _ ->
        assert_equal
          (Board.promote_pawn "C6" "G8" "Rook" b3 |> get_piece "G8")
          (Some Piece.(create White Rook)) );
      ( "test white Pawn to Knight from init" >:: fun _ ->
        assert_equal
          (Board.promote_pawn "G7" "D8" "Knight" b4 |> get_piece "D8")
          (Some Piece.(create White Knight)) );
    ]

  let tests =
    "Test pawn moves" >::: List.flatten [ test_white; test_black; test' ]
end

module TestPiece : T = struct
  open CommonTests
  open CommonPieces_Piece

  let tests =
    "Test pieces: get_piece & get_color"
    >::: List.flatten
           [
             test_piece black_rook Black Rook;
             test_piece black_pawn Black Pawn;
             test_piece white_knight White Knight;
             test_piece white_pawn White Pawn;
           ]
end

module TestMoveKing : T = struct
  let tests =
    "Test king logic"
    >::: [
           ( "test check_king UL" >:: fun _ ->
             assert (Board.legal_king_move "E3" "D4") );
           ( "test check_king UP" >:: fun _ ->
             assert (Board.legal_king_move "E3" "E4") );
           ( "test check_king UR" >:: fun _ ->
             assert (Board.legal_king_move "E3" "F4") );
           ( "test check_king LEFT" >:: fun _ ->
             assert (Board.legal_king_move "E3" "D3") );
           ( "test check_king RIGHT" >:: fun _ ->
             assert (Board.legal_king_move "E3" "F3") );
           ( "test check_king DL" >:: fun _ ->
             assert (Board.legal_king_move "E3" "D2") );
           ( "test check_king DOWN" >:: fun _ ->
             assert (Board.legal_king_move "E3" "E2") );
           ( "test check_king DR" >:: fun _ ->
             assert (Board.legal_king_move "E3" "F2") );
           ( "test check_king" >:: fun _ ->
             assert_equal false (Board.legal_king_move "E3" "E5") );
           ( "test check_king" >:: fun _ ->
             assert_equal false (Board.legal_king_move "E3" "E1") );
           ( "test check_king" >:: fun _ ->
             assert_equal false (Board.legal_king_move "E3" "C3") );
           ( "test check_king" >:: fun _ ->
             assert_equal false (Board.legal_king_move "E3" "G3") );
         ]
end

module TestToString : T = struct
  open CommonPieces_Piece

  let test1 =
    [
      ( "test black_rook" >:: fun _ ->
        assert_equal (Piece.to_string black_rook) "\u{265C}" );
      ( "test black_pawn" >:: fun _ ->
        assert_equal (Piece.to_string black_pawn) "\u{265F}" );
      ( "test white_knight" >:: fun _ ->
        assert_equal (Piece.to_string white_knight) "\u{2658}" );
      ( "test white_pawn" >:: fun _ ->
        assert_equal (Piece.to_string white_pawn) "\u{2659}" );
      ( "test black_king" >:: fun _ ->
        assert_equal
          (Piece.to_string (Option.get (Board.get_piece "E8" Board.initial)))
          "\u{265A}" );
      ( "test black_queen" >:: fun _ ->
        assert_equal
          (Piece.to_string (Option.get (Board.get_piece "D8" Board.initial)))
          "\u{265B}" );
      ( "test white_king" >:: fun _ ->
        assert_equal
          (Piece.to_string (Option.get (Board.get_piece "E1" Board.initial)))
          "\u{2654}" );
    ]

  let test2 =
    [
      ( "test white_queen" >:: fun _ ->
        assert_equal
          (Piece.to_string (Option.get (Board.get_piece "D1" Board.initial)))
          "\u{2655}" );
      ( "test black_bishop" >:: fun _ ->
        assert_equal
          (Piece.to_string (Option.get (Board.get_piece "C8" Board.initial)))
          "\u{265D}" );
      ( "test black_knight" >:: fun _ ->
        assert_equal
          (Piece.to_string (Option.get (Board.get_piece "B8" Board.initial)))
          "\u{265E}" );
      ( "test white_bishop" >:: fun _ ->
        assert_equal
          (Piece.to_string (Option.get (Board.get_piece "C1" Board.initial)))
          "\u{2657}" );
      ( "test white_rook" >:: fun _ ->
        assert_equal
          (Piece.to_string (Option.get (Board.get_piece "H1" Board.initial)))
          "\u{2656}" );
    ]

  let tests = "Test pieces: to_string" >::: List.flatten [ test1; test2 ]
end

module TestNameString : T = struct
  let tests =
    "Test pieces: name_string"
    >::: [
           ( "test name_queen" >:: fun _ ->
             assert_equal (Piece.name_string Queen) "Queen" );
           ( "test name_king" >:: fun _ ->
             assert_equal (Piece.name_string King) "King" );
           ( "test name_knight" >:: fun _ ->
             assert_equal (Piece.name_string Knight) "Knight" );
           ( "test name_rook" >:: fun _ ->
             assert_equal (Piece.name_string Rook) "Rook" );
           ( "test name_bishop" >:: fun _ ->
             assert_equal (Piece.name_string Bishop) "Bishop" );
           ( "test name_pawn" >:: fun _ ->
             assert_equal (Piece.name_string Pawn) "Pawn" );
         ]
end

module TestCheckOwn : T = struct
  let initial = Board.initial
  let b = Board.move_piece "A2" "A5" initial
  let b = Board.move_piece "G7" "H4" b

  let tests =
    "Test check_own"
    >::: [
           ( "new pos empty" >:: fun _ ->
             assert_equal (Board.check_own "A1" "F4" initial) false );
           ( "new pos opposite color" >:: fun _ ->
             assert_equal (Board.check_own "A1" "C7" initial) false );
           ( "new pos same color" >:: fun _ ->
             assert_equal (Board.check_own "A1" "G2" initial) true );
           ( "new pos diff color, not initial board" >:: fun _ ->
             assert_equal (Board.check_own "A5" "A7" b) false );
           ( "new pos same color, not initial board" >:: fun _ ->
             assert_equal (Board.check_own "H7" "H4" b) true );
           ( "new pos empty, not initial board" >:: fun _ ->
             assert_equal (Board.check_own "H4" "B5" b) false );
           ( "new pos empty bc piece was moved, not initial board" >:: fun _ ->
             assert_equal (Board.check_own "H4" "A2" b) false );
           ( "new pos empty bc piece was moved, not initial board" >:: fun _ ->
             assert_equal (Board.check_own "C1" "G7" b) false );
         ]
end

module TestCheckKingSafe : T = struct
  let initial = Board.initial
  let b1 = Board.move_piece "D7" "C6" initial
  let b1 = Board.move_piece "D2" "D7" b1
  let b2 = Board.move_piece "F2" "G3" initial
  let b2 = Board.move_piece "F7" "F2" b2
  let b3 = Board.move_piece "G8" "D3" initial
  let b4 = Board.move_piece "B1" "C7" initial

  let tests_1 =
    [
      ( "black king safe initially" >:: fun _ ->
        assert_equal (Board.king_is_safe initial Piece.Black) true );
      ( "white king safe initially" >:: fun _ ->
        assert_equal (Board.king_is_safe initial Piece.White) true );
      ( "black king not safe with pawn move" >:: fun _ ->
        assert_equal (Board.king_is_safe b1 Piece.Black) false );
      ( "white king still safe after pawn move" >:: fun _ ->
        assert_equal (Board.king_is_safe b1 Piece.White) true );
      ( "black king safe with pawn move" >:: fun _ ->
        assert_equal (Board.king_is_safe b2 Piece.Black) true );
      ( "white king not safe after pawn move" >:: fun _ ->
        assert_equal (Board.king_is_safe b2 Piece.White) false );
      ( "white king not safe after knight move" >:: fun _ ->
        assert_equal (Board.king_is_safe b3 Piece.White) false );
      ( "black king still safe after knight move" >:: fun _ ->
        assert_equal (Board.king_is_safe b3 Piece.Black) true );
      ( "black king not safe after knight move" >:: fun _ ->
        assert_equal (Board.king_is_safe b4 Piece.Black) false );
      ( "white king still safe after knight move" >:: fun _ ->
        assert_equal (Board.king_is_safe b4 Piece.White) true );
    ]

  let b5 = Board.move_piece "E2" "D3" initial
  let b5 = Board.move_piece "E8" "E2" b5
  let b6 = Board.move_piece "D1" "D7" initial
  let b6 = Board.move_piece "D8" "F1" b6
  let b7 = Board.move_piece "C8" "B4" initial
  let b7 = Board.move_piece "D2" "D3" b7
  let b7 = Board.move_piece "F1" "A4" b7
  let b7 = Board.move_piece "D7" "E6" b7
  let b8 = Board.move_piece "A8" "E5" initial
  let b8 = Board.move_piece "E2" "A3" b8
  let b8 = Board.move_piece "H1" "H8" b8
  let b8 = Board.move_piece "G8" "G7" b8
  let b8 = Board.move_piece "F8" "F7" b8

  let tests_2 =
    [
      ( "black king not safe with king move" >:: fun _ ->
        assert_equal (Board.king_is_safe b5 Piece.Black) false );
      ( "white king not safe with king move" >:: fun _ ->
        assert_equal (Board.king_is_safe b5 Piece.White) false );
      ( "black king not safe with queen move" >:: fun _ ->
        assert_equal (Board.king_is_safe b6 Piece.Black) false );
      ( "white king not safe with queen move" >:: fun _ ->
        assert_equal (Board.king_is_safe b6 Piece.White) false );
      ( "black king not safe with bishop move" >:: fun _ ->
        assert_equal (Board.king_is_safe b7 Piece.Black) false );
      ( "white king not safe with bishop move" >:: fun _ ->
        assert_equal (Board.king_is_safe b7 Piece.White) false );
      ( "black king not safe with rook move" >:: fun _ ->
        assert_equal (Board.king_is_safe b8 Piece.Black) false );
      ( "white king not safe with rook move" >:: fun _ ->
        assert_equal (Board.king_is_safe b8 Piece.White) false );
    ]

  let b9 = Board.move_piece "E2" "D5" initial
  let b9 = Board.move_piece "E7" "F5" b9
  let b10 = Board.move_piece "F8" "B5" b9
  let b10 = Board.move_piece "F1" "F8" b10
  let b11 = Board.move_piece "E8" "C5" b10
  let b12 = Board.move_piece "E1" "F3" b11

  let tests_3 =
    [
      ( "black king safe with rand move" >:: fun _ ->
        assert_equal (Board.king_is_safe b9 Piece.Black) true );
      ( "white king safe with rand move" >:: fun _ ->
        assert_equal (Board.king_is_safe b10 Piece.White) true );
      ( "black king not safe with rand move" >:: fun _ ->
        assert_equal (Board.king_is_safe b11 Piece.Black) false );
      ( "white king safe with rand move" >:: fun _ ->
        assert_equal (Board.king_is_safe b12 Piece.White) true );
    ]

  let tests =
    "Test king_is_safe" >::: List.flatten [ tests_1; tests_2; tests_3 ]
end

module TestLegalMove : T = struct
  let initial = Board.initial

  let tests =
    "Test if a move is legal"
    >::: [
           ( "moving a position with no piece is illegal" >:: fun _ ->
             assert_equal (Board.legal_move "C3" "C4" initial) false );
           ( "moving white piece to another white piece is illegal" >:: fun _ ->
             assert_equal (Board.legal_move "E1" "E2" initial) false );
           ( "moving black piece to another black piece is illegal" >:: fun _ ->
             assert_equal (Board.legal_move "A8" "A7" initial) false );
           ( "old position letter index must be 'A'-'H'" >:: fun _ ->
             assert_equal (Board.legal_move "I3" "C4" initial) false );
           ( "old position number index must be '1'-'8'" >:: fun _ ->
             assert_equal (Board.legal_move "A9" "C4" initial) false );
           ( "new position letter index must be 'A'-'H'" >:: fun _ ->
             assert_equal (Board.legal_move "D1" "Z8" initial) false );
           ( "new position number index must be '1'-'8'" >:: fun _ ->
             assert_equal (Board.legal_move "D1" "A9" initial) false );
           ( "testing a legal move" >:: fun _ ->
             assert_equal (Board.legal_move "D2" "D3" initial) true );
         ]
end

module TestCheckCastle : T = struct
  let initial = Board.initial
  let b1 = Board.move_piece "G1" "G3" initial
  let b2 = Board.move_piece "F1" "F3" b1
  let b3 = Board.move_piece "D1" "D3" b2
  let b4 = Board.move_piece "C1" "C3" b3
  let b5 = Board.move_piece "B1" "B3" b4

  let test_white_king_board =
    [
      ( "white king illegal move (jump over and lands on pieces)" >:: fun _ ->
        assert_equal (Board.check_castle "E1" "G1" initial) false );
      ( "white king illegal move (jump over a piece)" >:: fun _ ->
        assert_equal (Board.check_castle "E1" "G1" b1) false );
      ( "white king illegal move (lands on a piece)" >:: fun _ ->
        assert_equal (Board.check_castle "E1" "C1" b3) false );
      ( "white king illegal move (rook jump over a piece)" >:: fun _ ->
        assert_equal (Board.check_castle "E1" "C1" b4) false );
    ]

  let test_white_king_move =
    [
      ( "white king illegal move (incorrect layout, wrong new position)"
      >:: fun _ -> assert_equal (Board.check_castle "E1" "F1" initial) false );
      ( "white king illegal move (correct layout, wrong new position)"
      >:: fun _ -> assert_equal (Board.check_castle "E1" "C1" b2) false );
      ( "white king illegal move (incorrect layout, wrong new position)"
      >:: fun _ -> assert_equal (Board.check_castle "E1" "B1" b3) false );
      ( "white king illegal move (incorrect layout, wrong new position)"
      >:: fun _ -> assert_equal (Board.check_castle "E1" "B1" b4) false );
      ( "white king illegal move (correct layout, wrong new position)"
      >:: fun _ -> assert_equal (Board.check_castle "E1" "B1" b5) false );
    ]

  let b1' = Board.move_piece "G8" "G6" initial
  let b2' = Board.move_piece "F8" "F6" b1'
  let b3' = Board.move_piece "D8" "D6" b2'
  let b4' = Board.move_piece "C8" "C6" b3'
  let b5' = Board.move_piece "B8" "B6" b4'

  let test_black_king_board =
    [
      ( "black king illegal move (jump over and lands on pieces)" >:: fun _ ->
        assert_equal (Board.check_castle "E8" "G8" initial) false );
      ( "black king illegal move (jump over a piece)" >:: fun _ ->
        assert_equal (Board.check_castle "E8" "G8" b1') false );
      ( "black king illegal move (lands on a piece)" >:: fun _ ->
        assert_equal (Board.check_castle "E8" "C8" b3') false );
      ( "black king illegal move (rook jump over a piece)" >:: fun _ ->
        assert_equal (Board.check_castle "E8" "C8" b4') false );
    ]

  let test_black_king_move =
    [
      ( "black king illegal move (incorrect layout, wrong new position)"
      >:: fun _ -> assert_equal (Board.check_castle "E8" "F8" initial) false );
      ( "black king illegal move (correct layout, wrong new position)"
      >:: fun _ -> assert_equal (Board.check_castle "E8" "C8" b2') false );
      ( "black king illegal move (incorrect layout, wrong new position)"
      >:: fun _ -> assert_equal (Board.check_castle "E8" "B8" b3') false );
      ( "black king illegal move (incorrect layout, wrong new position)"
      >:: fun _ -> assert_equal (Board.check_castle "E8" "B8" b4') false );
      ( "black king illegal move (correct layout, wrong new position)"
      >:: fun _ -> assert_equal (Board.check_castle "E8" "B8" b5') false );
    ]

  let test_move_across =
    [
      ( "white king illegal move (move to black castle position)" >:: fun _ ->
        assert_equal (Board.check_castle "E1" "G8" b5') false );
      ( "black king illegal move (move to white castle position)" >:: fun _ ->
        assert_equal (Board.check_castle "E8" "C1" b5) false );
    ]

  let b6 = Board.move_piece "E1" "F1" b5
  let b7 = Board.move_piece "F1" "E1" b6
  let b6' = Board.move_piece "E8" "F8" b5'
  let b7' = Board.move_piece "F8" "E8" b6'

  let test_white_has_moved =
    [
      ( "white king has moved (not back to inital position) 1" >:: fun _ ->
        assert_equal (Board.check_castle "F1" "G1" b6) false );
      ( "white king has moved (back to inital position) 1" >:: fun _ ->
        assert_equal (Board.check_castle "E1" "G1" b7) false );
      ( "white king has moved (not back to inital position) 2" >:: fun _ ->
        assert_equal (Board.check_castle "F1" "C1" b6) false );
      ( "white king has moved (back to inital position) 2" >:: fun _ ->
        assert_equal (Board.check_castle "E1" "C1" b7) false );
    ]

  let test_black_has_moved =
    [
      ( "black king has moved (not back to inital position) 1" >:: fun _ ->
        assert_equal (Board.check_castle "F8" "G8" b6') false );
      ( "black king has moved (back to inital position) 1" >:: fun _ ->
        assert_equal (Board.check_castle "E8" "G8" b7') false );
      ( "black king has moved (not back to inital position) 2" >:: fun _ ->
        assert_equal (Board.check_castle "F8" "C8" b6') false );
      ( "black king has moved (back to inital position) 2" >:: fun _ ->
        assert_equal (Board.check_castle "E8" "C8" b7') false );
    ]

  let tests =
    "Test check castle"
    >::: List.flatten
           [
             test_white_king_board;
             test_white_king_move;
             test_black_king_board;
             test_black_king_move;
             test_move_across;
             test_white_has_moved;
             test_black_has_moved;
           ]
end

module TestCastle : T = struct
  let initial = Board.initial
  let b1 = Board.move_piece "G1" "G3" initial |> Board.move_piece "F1" "F3"

  let b2 =
    Board.move_piece "D1" "D3" initial
    |> Board.move_piece "C1" "C3" |> Board.move_piece "B1" "B3"

  let test_white_castle =
    [
      ( "White castle G (check king)" >:: fun _ ->
        assert_equal
          (Board.castle "E1" "G1" b1 |> Board.get_piece "G1")
          (Some Piece.(create White King)) );
      ( "White castle (check rook)" >:: fun _ ->
        assert_equal
          (Board.castle "E1" "G1" b1 |> Board.get_piece "F1")
          (Some Piece.(create White Rook)) );
      ( "White castle C (check king)" >:: fun _ ->
        assert_equal
          (Board.castle "E1" "C1" b2 |> Board.get_piece "C1")
          (Some Piece.(create White King)) );
      ( "White castle C (check rook)" >:: fun _ ->
        assert_equal
          (Board.castle "E1" "C1" b2 |> Board.get_piece "D1")
          (Some Piece.(create White Rook)) );
    ]

  let test_black_castle =
    [
      ( "Black castle G (check king)" >:: fun _ ->
        assert_equal
          (Board.castle "E8" "G8" b1 |> Board.get_piece "G8")
          (Some Piece.(create Black King)) );
      ( "Black castle (check rook)" >:: fun _ ->
        assert_equal
          (Board.castle "E8" "G8" b1 |> Board.get_piece "F8")
          (Some Piece.(create Black Rook)) );
      ( "Black castle C (check king)" >:: fun _ ->
        assert_equal
          (Board.castle "E8" "C8" b2 |> Board.get_piece "C8")
          (Some Piece.(create Black King)) );
      ( "Black castle C (check rook)" >:: fun _ ->
        assert_equal
          (Board.castle "E8" "C8" b2 |> Board.get_piece "D8")
          (Some Piece.(create Black Rook)) );
    ]

  let tests =
    "Test castle" >::: List.flatten [ test_white_castle; test_black_castle ]
end

module TestCheckMate : T = struct
  open Board

  (* Simple test *)
  let b1 =
    initial |> move_piece "F2" "F3" |> move_piece "E7" "E5"
    |> move_piece "G2" "G4" |> move_piece "D8" "H4"

  (* Tail Mate (Swallow's Tail Checkmate) *)
  let b2 =
    empty
    |> add_piece (Piece.create Black Pawn) "F7"
    |> add_piece (Piece.create Black Pawn) "H7"
    |> add_piece (Piece.create Black King) "G6"
    |> add_piece (Piece.create White Queen) "G5"
    |> add_piece (Piece.create White Pawn) "H4"
    |> add_piece (Piece.create Black Queen) "C2"
    |> add_piece (Piece.create White King) "G1"

  let b3' =
    empty
    |> add_piece (Piece.create Black Pawn) "G7"
    |> add_piece (Piece.create Black King) "H7"
    |> add_piece (Piece.create White Rook) "H3"
    |> add_piece (Piece.create White King) "G1"

  (* Anastasia's Mate *)
  let b3 = b3' |> add_piece (Piece.create White Knight) "E7"

  (* Anderssen's Mate *)
  let b4 =
    empty
    |> add_piece (Piece.create Black King) "G8"
    |> add_piece (Piece.create White Pawn) "G7"
    |> add_piece (Piece.create White Rook) "H8"
    |> add_piece (Piece.create White King) "F6"

  let b4' = b4 |> move_piece "F6" "F5"

  let b5' =
    empty
    |> add_piece (Piece.create White King) "H8"
    |> add_piece (Piece.create Black Rook) "H7"
    |> add_piece (Piece.create Black King) "G1"

  (* Arabian Mate *)
  let b5 = b5' |> add_piece (Piece.create Black Knight) "F6"

  let b6' =
    empty
    |> add_piece (Piece.create White King) "G1"
    |> add_piece (Piece.create White Pawn) "F2"
    |> add_piece (Piece.create White Pawn) "G2"
    |> add_piece (Piece.create Black Rook) "D1"
    |> add_piece (Piece.create Black King) "G8"

  (* Back Rank Mate*)
  let b6 = b6' |> add_piece (Piece.create White Pawn) "H2"

  let b7' =
    empty
    |> add_piece (Piece.create White King) "G1"
    |> add_piece (Piece.create White Bishop) "C6"
    |> add_piece (Piece.create Black King) "E8"

  (* Balestra Mate *)
  let b7 = b7' |> add_piece (Piece.create White Queen) "F6"

  let b8' =
    empty
    |> add_piece (Piece.create White King) "G1"
    |> add_piece (Piece.create White Bishop) "H7"
    |> add_piece (Piece.create Black King) "G8"
    |> add_piece (Piece.create Black Rook) "F8"
    |> add_piece (Piece.create White Knight) "G5"

  (* Blackburne's Mate *)
  let b8 = b8' |> add_piece (Piece.create White Bishop) "B2"

  let tests =
    "Test if king is in checkmate"
    >::: [
           ( "initial - white safe" >:: fun _ ->
             assert_equal (king_in_checkmate initial White) false );
           ( "initial - black safe" >:: fun _ ->
             assert_equal (king_in_checkmate initial Black) false );
           ( "b1 - white in checkmate" >:: fun _ ->
             assert_equal (king_in_checkmate b1 White) true );
           ( "b1 - black safe" >:: fun _ ->
             assert_equal (king_in_checkmate b1 Black) false );
           ( "b2 - black in checkmate" >:: fun _ ->
             assert_equal (king_in_checkmate b2 Black) true );
           ( "b2 - white safe" >:: fun _ ->
             assert_equal (king_in_checkmate b2 White) false );
           ( "b3 - black in checkmate" >:: fun _ ->
             assert_equal (king_in_checkmate b3 Black) true );
           ( "b3 - white safe" >:: fun _ ->
             assert_equal (king_in_checkmate b3 White) false );
           ( "b3' - black in check but NOT checkmate" >:: fun _ ->
             assert_equal (king_in_checkmate b3' Black) false );
           ( "b3' - white safe" >:: fun _ ->
             assert_equal (king_in_checkmate b3' White) false );
           ( "b4 - black in checkmate" >:: fun _ ->
             assert_equal (king_in_checkmate b4 Black) true );
           ( "b4 - white safe" >:: fun _ ->
             assert_equal (king_in_checkmate b4 White) false );
           ( "b4' - black in check but NOT checkmate" >:: fun _ ->
             assert_equal (king_in_checkmate b4' Black) false );
           ( "b4' - white safe" >:: fun _ ->
             assert_equal (king_in_checkmate b4' White) false );
           ( "b5 - white in checkmate" >:: fun _ ->
             assert_equal (king_in_checkmate b5 White) true );
           ( "b5 - black safe" >:: fun _ ->
             assert_equal (king_in_checkmate b5 Black) false );
           ( "b5' - white in check but NOT checkmate" >:: fun _ ->
             assert_equal (king_in_checkmate b5' White) false );
           ( "b5' - black safe" >:: fun _ ->
             assert_equal (king_in_checkmate b5' Black) false );
           ( "b6 - white in checkmate" >:: fun _ ->
             assert_equal (king_in_checkmate b6 White) true );
           ( "b6 - black safe" >:: fun _ ->
             assert_equal (king_in_checkmate b6 Black) false );
           ( "b6' - white in check but NOT checkmate" >:: fun _ ->
             assert_equal (king_in_checkmate b6' White) false );
           ( "b6' - black safe" >:: fun _ ->
             assert_equal (king_in_checkmate b6' Black) false );
           ( "b7 - black in checkmate" >:: fun _ ->
             assert_equal (king_in_checkmate b7 Black) true );
           ( "b7 - white safe" >:: fun _ ->
             assert_equal (king_in_checkmate b7 White) false );
           ( "b7' - black in check but NOT checkmate" >:: fun _ ->
             assert_equal (king_in_checkmate b7' Black) false );
           ( "b7' - white safe" >:: fun _ ->
             assert_equal (king_in_checkmate b7' White) false );
           ( "b8 - black in checkmate" >:: fun _ ->
             assert_equal (king_in_checkmate b8 Black) true );
           ( "b8 - white safe" >:: fun _ ->
             assert_equal (king_in_checkmate b8 White) false );
           ( "b8' - black in check but NOT checkmate" >:: fun _ ->
             assert_equal (king_in_checkmate b8' Black) false );
           ( "b8' - white safe" >:: fun _ ->
             assert_equal (king_in_checkmate b8' White) false );
         ]
end

module TestPrintLegalMoves : T = struct
  let initial = Board.initial
  let b1 = Board.move_piece "D2" "D4" initial
  let b2 = Board.move_piece "D7" "D5" b1
  let b3 = Board.move_piece "D1" "D3" b2
  let mid_game_board = Board.move_piece "A7" "A5" b3

  let tests =
    "TestPrintLegalMoves"
    >::: [
           ( "test A2 pawn" >:: fun _ ->
             assert_equal (Board.legal_moves_list "A2" initial) [ "A4"; "A3" ]
           );
           ( "test H1 rook" >:: fun _ ->
             assert_equal (Board.legal_moves_list "H1" initial) [] );
           ( "test B8 knight" >:: fun _ ->
             assert_equal (Board.legal_moves_list "B8" initial) [ "A6"; "C6" ]
           );
           ( "test G7 pawn" >:: fun _ ->
             assert_equal (Board.legal_moves_list "G7" initial) [ "G6"; "G5" ]
           );
           ( "test D3 queen" >:: fun _ ->
             assert_equal
               (Board.legal_moves_list "D3" mid_game_board)
               [
                 "H7";
                 "A6";
                 "G6";
                 "B5";
                 "F5";
                 "C4";
                 "E4";
                 "A3";
                 "B3";
                 "C3";
                 "E3";
                 "F3";
                 "G3";
                 "H3";
                 "D2";
                 "D1";
               ] );
           ( "test C1 bishop" >:: fun _ ->
             assert_equal
               (Board.legal_moves_list "C1" mid_game_board)
               [ "H6"; "G5"; "F4"; "E3"; "D2" ] );
           ( "test E1 king" >:: fun _ ->
             assert_equal
               (Board.legal_moves_list "E1" mid_game_board)
               [ "D2"; "D1" ] );
           ( "test A8 rook" >:: fun _ ->
             assert_equal
               (Board.legal_moves_list "A8" mid_game_board)
               [ "A7"; "A6" ] );
         ]
end

let _ = run_test_tt_main TestBoardInit.tests
let _ = run_test_tt_main TestGetPiece.tests
let _ = run_test_tt_main TestPiece.tests
let _ = run_test_tt_main TestCreatePiece.tests
let _ = run_test_tt_main TestMovePiece.tests
let _ = run_test_tt_main TestToString.tests
let _ = run_test_tt_main TestNameString.tests
let _ = run_test_tt_main TestScore.tests
let _ = run_test_tt_main TestMoveBishop.tests
let _ = run_test_tt_main TestMoveRook.tests
let _ = run_test_tt_main TestMoveQueen.tests
let _ = run_test_tt_main TestMoveKnight.tests
let _ = run_test_tt_main TestMovePawn.tests
let _ = run_test_tt_main TestMoveKing.tests
let _ = run_test_tt_main TestCheckOwn.tests
let _ = run_test_tt_main TestCheckKingSafe.tests
let _ = run_test_tt_main TestLegalMove.tests
let _ = run_test_tt_main TestCheckCastle.tests
let _ = run_test_tt_main TestCastle.tests
let _ = run_test_tt_main TestPawnPromotion.tests
let _ = run_test_tt_main TestLegalPos.tests
let _ = run_test_tt_main TestCheckMate.tests
let _ = run_test_tt_main TestPrintLegalMoves.tests
