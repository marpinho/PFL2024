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
    not(piece_occupying_position(Pieces, Row, Col, Position)).  % Ensure no piece is occupying the position.

% piece_occupying_position(+Pieces, +Row, +Col, +Position)
% Checks if there is a piece occupying the given position.
piece_occupying_position(Pieces, Row, Col, Position) :-
    member(piece(_, Row, Col, Position), Pieces).

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
accessible(player1Exclusive, Player1) :-    % Exclusive squares for Player1.
    game_state(_, _, Player1, _, _).
accessible(player2Exclusive, Player2) :-    % Exclusive squares for Player2.
    game_state(_, _, Player2, _, _).
accessible(_, _) :- fail.                   % Other types are inaccessible.

% section_blossom_type(+Section, +TL, +TR, +BL, +BR, -BlossomType)
section_blossom_type(topLeft, TL, _, _, _, TL).
section_blossom_type(topRight, _, TR, _, _, TR).
section_blossom_type(bottomLeft, _, _, BL, _, BL).
section_blossom_type(bottomRight, _, _, _, BR, BR).

% valid_bl_type(+Player, +BlossomType)
valid_bl_type(player1, neutral).
valid_bl_type(player1, player1Exclusive).
valid_bl_type(player2, neutral).
valid_bl_type(player2, player2Exclusive).
valid_bl_type(_, inaccessible) :- fail.  % Prevent movement to inaccessible squares.

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
    valid_square(Board, Player, Row, Col, Position, Pieces).
validate_starting_position(_, _, _, _, _, _, false).
% Validate starting position

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


