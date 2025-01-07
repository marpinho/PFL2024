:- use_module(library(between)).

% valid_moves(+GameState, -ListOfMoves)
% Determines the list of valid moves based on the current phase.
valid_moves(game_state(Board, Pieces, CurrentPlayer, CurrentPhase, _RemainingMoves), ListOfMoves) :-
    valid_rotation_moves(CurrentPhase, RotationMoves),
    valid_piece_moves(CurrentPhase, Board, Pieces, CurrentPlayer, PieceMoves),  % Add piece moves.
    append(RotationMoves, PieceMoves, ListOfMoves).  % Combine lists.

% valid_square(+Board, +Player, +Row, +Col, +Position, +Pieces)
% Checks if the given position on the board is valid for the player and unoccupied.
valid_square(Board, Player, Row, Col, Position, Pieces) :-
    nth1(Row, Board, RowList),                   % Extract the Row
    nth1(Col, RowList, Disc),                    % Extract the Disc at Column
    square_accessible(Disc, Player, Position),   % Check if the Position is accessible
    \+ piece_occupying_position(Pieces, Row, Col, Position).  % Ensure no piece is occupying the position.

% square_accessible(+Disc, +Player, +Position)
% Checks if the given position on the disc is accessible to the player.
square_accessible(disc(Position1, _, _, _), Player, topLeft) :-
    accessible(Position1, Player).
square_accessible(disc(_, Position2, _, _), Player, topRight) :-
    accessible(Position2, Player).
square_accessible(disc(_, _, Position3, _), Player, bottomLeft) :-
    accessible(Position3, Player).
square_accessible(disc(_, _, _, Position4), Player, bottomRight) :-
    accessible(Position4, Player).

% accessible(+SquareType, +Player)
% Determines if a square type is accessible to the player.
accessible(neutral, _).                     % Neutral squares are accessible to all players.
accessible(player1Exclusive, player1).      % Exclusive squares for Player1.
accessible(player2Exclusive, player2).      % Exclusive squares for Player2.
accessible(_, _) :- fail.                   % Other types are inaccessible.

% piece_occupying_position(+Pieces, +Row, +Col, +Slot)
% Checks if a piece occupies the specified position.
piece_occupying_position(Pieces, Row, Col, Slot) :-
    member(piece(_, Row, Col, Slot), Pieces).

/* ROTATION */

% valid_rotation_moves(+CurrentPhase, -RotationMoves)
valid_rotation_moves(rotate, RotationMoves) :- % Rotation mode: return the 16 possible rotations.
    findall(move(rotation, row(RowIndex)), between(1, 4, RowIndex), RowMoves),  % All row rotations.
    findall(move(rotation, col(ColIndex)), between(1, 4, ColIndex), ColMoves),  % All column rotations.
    append(RowMoves, ColMoves, RotationMoves).
valid_rotation_moves(movePiece, []). % No rotations allowed during the movePiece phase.

/* PLACE A NEW PIECE */

% Validate starting position
validate_starting_position(Player, Board, Row, Col, Position, Pieces, true) :-
    player_row(Player, RequiredRow),                    % Get the required row for the player
    Row = RequiredRow,                                  % Ensure the row matches the player starting row
    valid_square(Board, Player, Row, Col, Position, Pieces).
validate_starting_position(_, _, _, _, _, _, false).

% player_row(+Player, -Row)
% Defines the required row for each player.
player_row(player1, 1). % Player 1 starts in row 1.
player_row(player2, 4). % Player 2 starts in row 4.

/* MOVE A PLACED PIECE */

% Validate movement action
validate_movement_action(Player, Board, Row, Col, Direction, true) :-
    valid_square(Board, Player, Row, Col, Direction, Pieces). % Pass Pieces dynamically.
validate_movement_action(_, _, _, _, _, false).

% valid_piece_moves(+CurrentPhase, +Board, +Pieces, +CurrentPlayer, -PieceMoves)
valid_piece_moves(movePiece, Board, Pieces, CurrentPlayer, PieceMoves) :-
    findall(move(movement, (Row, Col), Direction), valid_piece_move(Board, Pieces, CurrentPlayer, (Row, Col), Direction), PieceMoves).
valid_piece_moves(rotate, _, _, _, []).  % No piece moves allowed during the rotate phase.

valid_piece_move(Board, Pieces, CurrentPlayer, (Row, Col), Direction) :-
    member(piece(CurrentPlayer, Row, Col, Section), Pieces),  % Get the player piece.
    Row \= none, Col \= none,  % Ensure its placed on the board.
    adjacent_position(Section, Row, Col, Direction, NewSection, NewRow, NewCol),  % Get new section/position.
    NewSection \= none,  % Ensure the new section exists (no "up" from topLeft, etc.).
    valid_square(Board, Pieces, CurrentPlayer, NewRow, NewCol, NewSection).  % Check the target square.

adjacent_position(topLeft, Row, Col, right, topRight, Row, Col).  % Move to topRight within the same disc.
adjacent_position(topLeft, Row, Col, left, topRight, Row, NewCol) :-  % Move to left disc topRight.
    NewCol is Col - 1, NewCol > 0.
adjacent_position(topLeft, _, _, up, none, _, _).  % No valid move upwards from topLeft.
adjacent_position(topLeft, Row, Col, down, bottomRight, Row, Col).  % Move to bottomRight within the same disc.

adjacent_position(topRight, Row, Col, right, topLeft, Row, NewCol) :-  % Move to right discs topLeft.
    NewCol is Col + 1, NewCol =< 4.
adjacent_position(topRight, Row, Col, left, topLeft, Row, Col).  % Move to topLeft within the same disc.
adjacent_position(topRight, _, _, up, none, _, _).  % No valid move upwards from topRight.
adjacent_position(topRight, Row, Col, down, bottomLeft, Row, Col).  % Move to bottomLeft within the same disc.

adjacent_position(bottomLeft, Row, Col, right, bottomRight, Row, Col).  % Move to bottomRight within the same disc.
adjacent_position(bottomLeft, Row, Col, left, bottomRight, Row, NewCol) :-  % Move to left disc bottomRight.
    NewCol is Col - 1, NewCol > 0.
adjacent_position(bottomLeft, Row, Col, up, topRight, Row, Col).  % Move to topRight within the same disc.
adjacent_position(bottomLeft, _, _, down, none, _, _).  % No valid move downwards from bottomLeft.

adjacent_position(bottomRight, Row, Col, right, bottomLeft, Row, NewCol) :-  % Move to right disc bottomLeft.
    NewCol is Col + 1, NewCol =< 4.
adjacent_position(bottomRight, Row, Col, left, bottomLeft, Row, Col).  % Move to bottomLeft within the same disc.
adjacent_position(bottomRight, Row, Col, up, topLeft, Row, Col).  % Move to topLeft within the same disc.
adjacent_position(bottomRight, _, _, down, none, _, _).  % No valid move downwards from bottomRight.


% piece_on_board(+Row, +Col)
% Ensures the piece is already placed on the board (i.e., not unplaced).
piece_on_board(Row, Col) :-
    Row \= none,
    Col \= none.