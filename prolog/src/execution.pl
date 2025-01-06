% 2- Movement

% Execute move
execute_move(game_state(Board, Pieces, CurrentPlayer, rotate, MovesLeft, Player1, Player2), move(rotation, Target), game_state(NewBoard, NewPieces, CurrentPlayer, move, 3, Player1, Player2)) :-
    rotate_target(game_state(Board, Pieces, CurrentPlayer, rotate, MovesLeft, Player1, Player2), Target, game_state(NewBoard, NewPieces, CurrentPlayer, move, 3, Player1, Player2)).
execute_move(game_state(Board, Pieces, CurrentPlayer, move, MovesLeft, Player1, Player2), move(movement, Piece, Path), game_state(Board, NewPieces, CurrentPlayer, move, NewMovesLeft, Player1, Player2)) :-
    move_piece(game_state(Board, Pieces, CurrentPlayer, move, MovesLeft, Player1, Player2), Piece, Path, game_state(Board, NewPieces, CurrentPlayer, move, NewMovesLeft, Player1, Player2)).


% 2.X - Rotation

% Rotate a row or column and update the game state
rotate_target(game_state(Board, Pieces, Player, rotate, _, Player1, Player2), row(Row), game_state(NewBoard, UpdatedPieces, Player, move, 3, Player1, Player2)) :-
    nth1(Row, Board, OldRow),                          % Extract the row to rotate
    rotate_row_discs(OldRow, RotatedRow, Pieces, TempPieces), % Rotate discs and update pieces
    replace_row(Board, Row, RotatedRow, NewBoard),     % Replace the row in the board
    UpdatedPieces = TempPieces.

rotate_target(game_state(Board, Pieces, Player, rotate, _, Player1, Player2), col(Col), game_state(NewBoard, UpdatedPieces, Player, move, 3, Player1, Player2)) :-
    extract_column(Board, Col, OldColumn),             % Extract the column to rotate
    rotate_row_discs(OldColumn, RotatedColumn, Pieces, TempPieces), % Rotate discs and update pieces
    replace_column(Board, Col, RotatedColumn, NewBoard), % Replace the column in the board
    UpdatedPieces = TempPieces.



% Rotate discs in a row or column and update piece positions
rotate_row_discs([], [], Pieces, Pieces).
rotate_row_discs([Disc | RestDiscs], [RotatedDisc | RestRotated], Pieces, UpdatedPieces) :-
    rotate_disc(Disc, RotatedDisc),                    % Rotate the current disc
    update_pieces_on_disc(Pieces, Disc, RotatedDisc, TempPieces), % Update pieces on the rotated disc
    rotate_row_discs(RestDiscs, RestRotated, TempPieces, UpdatedPieces). % Process the remaining discs


% Update positions of pieces on a rotated disc
update_pieces_on_disc([], _, _, []).
update_pieces_on_disc([piece(Player, Row, Col, Pos) | Rest], OldDisc, NewDisc, [piece(Player, Row, Col, NewPos) | UpdatedRest]) :-
    nth1(PosIndex, [topLeft, topRight, bottomRight, bottomLeft], Pos), % Map position to an index
    nth1(NewPosIndex, [topLeft, topRight, bottomRight, bottomLeft], NewPos), % Map rotated index to new position
    rotate_position(PosIndex, NewPosIndex), % Rotate the index
    update_pieces_on_disc(Rest, OldDisc, NewDisc, UpdatedRest).
update_pieces_on_disc([Piece | Rest], OldDisc, NewDisc, [Piece | UpdatedRest]) :-
    update_pieces_on_disc(Rest, OldDisc, NewDisc, UpdatedRest). % Keep other pieces unchanged


% Rotate a position index on a disc 90° clockwise
rotate_position(1, 2). % topLeft -> topRight
rotate_position(2, 3). % topRight -> bottomRight
rotate_position(3, 4). % bottomRight -> bottomLeft
rotate_position(4, 1). % bottomLeft -> topLeft

% Rotate a position on a disc 90° clockwise
rotate_position(topLeft, topRight).
rotate_position(topRight, bottomRight).
rotate_position(bottomRight, bottomLeft).
rotate_position(bottomLeft, topLeft).


% Discs are always rotated 90º clockwise, Rows or Cols only matter in defining the list of disks to rotate
% Rotate a disc
rotate_disc(disc(TL, TR, BL, BR), disc(BL, TL, BR, TR)).




% Replace row or column
replace_row(Board, RowIndex, NewRow, NewBoard) :-
    nth1(RowIndex, Board, _, RestBoard),
    nth1(RowIndex, NewBoard, NewRow, RestBoard).

% replace_column(+Board, +ColIndex, +NewColumn, -NewBoard)
replace_column(Board, ColIndex, NewCol, NewBoard) :-
    transpose(Board, TransposedBoard),
    replace_row(TransposedBoard, ColIndex, NewCol, NewTransposedBoard),
    transpose(NewTransposedBoard, NewBoard).

% extract_column(+Board, +ColIndex, -Column)
extract_column([], _, []). % Base Case: All rows processed
extract_column([Row | Rest], ColIndex, [Disc | ColumnRest]) :- % Recursive Step: Extract the disk in the col index from a row.
    nth1(ColIndex, Row, Disc),
    extract_column(Rest, ColIndex, ColumnRest).

% Place a new piece on the board
place_piece(Player, Row, Col, Pos, Pieces, [piece(Player, Row, Col, Pos) | Pieces]).

% Move a piece
move_piece(game_state(Board, Pieces, Player, move, MovesLeft, Player1, Player2), piece(Player, Row, Col, Pos), [exit], game_state(Board, NewPieces, Player, move, NewMovesLeft, Player1, Player2)) :-
    valid_exit(Board, Player, Row, Col, Pos),
    mark_piece_off_board(Pieces, piece(Player, Row, Col, Pos), NewPieces),
    NewMovesLeft is MovesLeft - 1.
move_piece(game_state(Board, Pieces, Player, move, MovesLeft, Player1, Player2), piece(Player, Row, Col, Pos), [NextPos | Path], game_state(Board, NewPieces, Player, move, NewMovesLeft, Player1, Player2)) :-
    update_piece_position(Pieces, piece(Player, Row, Col, Pos), NextPos, UpdatedPieces),
    move_piece(game_state(Board, UpdatedPieces, Player, move, MovesLeft - 1, Player1, Player2), piece(Player, Row, Col, NextPos), Path, game_state(Board, NewPieces, Player, move, NewMovesLeft, Player1, Player2)).
move_piece(GameState, _, [], GameState).

% Update piece position
update_piece_position([piece(Player, Row, Col, Pos) | Rest], piece(Player, Row, Col, Pos), NewPos, [piece(Player, Row, Col, NewPos) | Rest]).
update_piece_position([OtherPiece | Rest], Piece, NewPos, [OtherPiece | UpdatedRest]) :-
    update_piece_position(Rest, Piece, NewPos, UpdatedRest).

% Mark a piece as off the board
mark_piece_off_board([piece(Player, Row, Col, Pos) | Rest], piece(Player, Row, Col, Pos), [piece(Player, off, off, Pos) | Rest]).
mark_piece_off_board([OtherPiece | Rest], Piece, [OtherPiece | UpdatedRest]) :-
    mark_piece_off_board(Rest, Piece, UpdatedRest).

% Validate exit move
valid_exit(Board, Player, Row, Col, Pos) :-
    Player == player1, Row = 4, valid_square(Board, Player, Row, Col, Pos).
valid_exit(Board, Player, Row, Col, Pos) :-
    Player == player2, Row = 1, valid_square(Board, Player, Row, Col, Pos).
