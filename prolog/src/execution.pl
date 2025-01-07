:- use_module(library(random)).

/* GENERATE BOARD*/
% Generate a 4x4 board with randomized discs
generate_board(Board) :-
    length(FlatBoard, 16),  % Create a list of 16 discs
    maplist(random_disk, FlatBoard),  % Generate 16 random discs
    partition(4, FlatBoard, Board).

% Randomly generate a disk with one of each slot type in random order
random_disk(disc(Slot1, Slot2, Slot3, Slot4)) :-
    random_permutation([player1Exclusive, player2Exclusive, neutral, inaccessible], [Slot1, Slot2, Slot3, Slot4]).

% Partition a flat list into a 4x4 board
partition(_, [], []).
partition(Size, List, [Row|Rest]) :-
    length(Row, Size),
    append(Row, Tail, List),
    partition(Size, Tail, Rest).

% Initialize player pieces
initialize_pieces(Pieces) :-
    initialize_player_pieces(player1, Player1Pieces),
    initialize_player_pieces(player2, Player2Pieces),
    append(Player1Pieces, Player2Pieces, Pieces).

initialize_player_pieces(Player, Pieces) :-
    length(Pieces, 5),
    maplist(=(piece(Player, none, none, none)), Pieces).

/* Execute move */

move(game_state(Board, Pieces, CurrentPlayer, rotate, MovesLeft), move(rotation, row(Index)), game_state(NewBoard, Pieces, CurrentPlayer, move, 3)) :-
    rotate_row(Board, Index, NewBoard, OldPieces, NewPieces).

move(game_state(Board, Pieces, CurrentPlayer, rotate, MovesLeft), move(rotation, col(Index)), game_state(NewBoard, Pieces, CurrentPlayer, move, 3)) :-
    rotate_column(Board, Index, NewBoard, OldPieces, NewPieces).

move(game_state(Board, Pieces, CurrentPlayer, move, MovesLeft), move(movement, Piece, Direction), game_state(Board, NewPieces, CurrentPlayer, move, NewMovesLeft)) :-
    move_piece(game_state(Board, Pieces, CurrentPlayer, move, MovesLeft), Piece, Direction, game_state(Board, NewPieces, Player, move, NewMovesLeft)).


/* ROTATION */

% Rotate a row or column and update the game state
rotate_row(Board, Index, NewBoard, OldPieces, NewPieces) :-
    nth1(Index, Board, Row),
    rotate_all_disks(Row, RotatedRow),
    replace_row(Board, Index, RotatedRow, NewBoard),
    pieces_in_row(OldPieces, Index, PiecesInRow),
    rotate_pieces_90_clockwise(PiecesInRow, RotatedPieces),
    replace_rotated_pieces(OldPieces, RotatedPieces, NewPieces).


% Rotate a column and update all the pieces affected by the rotation
rotate_column(Board, Index, NewBoard, OldPieces, UpdatedPieces) :-
    transpose(Board, TransposedBoard),                     % Transpose to treat column as row
    nth1(Index, TransposedBoard, Column),                  % Get the target column
    rotate_all_disks(Column, RotatedColumn),               % Rotate all disks in the column
    replace_row(TransposedBoard, Index, RotatedColumn, RotatedTransposedBoard), % Replace the rotated column
    transpose(RotatedTransposedBoard, NewBoard),           % Transpose back to restore board orientation
    pieces_in_column(OldPieces, Index, PiecesInColumn),    % Extract pieces in the target column
    rotate_pieces_90_clockwise(PiecesInColumn, RotatedPieces), % Rotate their positions
    replace_rotated_pieces(OldPieces, RotatedPieces, UpdatedPieces). % Update the full list of pieces

% rotate_all_disks(+Disks, -RotatedDisks)
% Rotates all disks in the input list.
rotate_all_disks([], []). % Base case: An empty list produces an empty list.
rotate_all_disks([Disk | Rest], [RotatedDisk | RotatedRest]) :-
    rotate_disk(Disk, RotatedDisk),  % Rotate the current disk
    rotate_all_disks(Rest, RotatedRest). % Recursively rotate the rest of the list.

% rotate_disk(+OldDisk, -NewDisk)
% Rotates a single disk clockwise.
rotate_disk(disc(TL, TR, BL, BR), disc(BL, TL, BR, TR)).

% pieces_in_row(+Pieces, +Row, -PiecesInRow)
% Retrieves all pieces located in the specified row.
pieces_in_row(Pieces, Row, PiecesInRow) :-
    findall(
        piece(Player, Row, Col, Slot),
        member(piece(Player, Row, Col, Slot), Pieces),
        PiecesInRow
    ).

% pieces_in_column(+Pieces, +Col, -PiecesInColumn)
% Retrieves all pieces located in the specified column.
pieces_in_column(Pieces, Col, PiecesInColumn) :-
    findall(
        piece(Player, Row, Col, Slot),
        member(piece(Player, Row, Col, Slot), Pieces),
        PiecesInColumn
    ).

% rotate_pieces_90_clockwise(+Pieces, -RotatedPieces)
% Rotates all pieces in the input list 90 degrees clockwise.
rotate_pieces_90_clockwise([], []). % Base case: An empty list produces an empty list.
rotate_pieces_90_clockwise([piece(Player, Row, Col, OldSlot) | Rest], [piece(Player, Row, Col, NewSlot) | RotatedRest]) :-
    rotate_piece_90_clockwise(OldSlot, NewSlot), % Rotate the pieces position
    rotate_pieces_90_clockwise(Rest, RotatedRest). % Recursively rotate the rest of the list.

% rotate_piece_90_clockwise(+OldSlot, -NewSlot)
% Rotates a single pieces position 90 degrees clockwise.
rotate_piece_90_clockwise(topLeft, topRight).
rotate_piece_90_clockwise(topRight, bottomRight).
rotate_piece_90_clockwise(bottomLeft, topRight).
rotate_piece_90_clockwise(bottomRight, bottomLeft).


replace_row(Board, Index, NewRow, NewBoard) :-
    nth1(Index, Board, _, Rest),
    nth1(Index, NewBoard, NewRow, Rest).

% Replace rotated pieces in the full list of all pieces
replace_rotated_pieces(AllPieces, RotatedPieces, UpdatedPieces) :-
    exclude(is_rotated_piece(RotatedPieces), AllPieces, NonRotatedPieces),
    append(RotatedPieces, NonRotatedPieces, UpdatedPieces).

% Check if a piece belongs to the list of rotated pieces
is_rotated_piece(RotatedPieces, piece(Player, Row, Col, _)) :-
    member(piece(Player, Row, Col, _), RotatedPieces).


/* PLACE A NEW PIECE */

% Place a new piece on the board
place_piece(Player, Row, Col, Pos, Pieces, [piece(Player, Row, Col, Pos) | Pieces]).

% place_piece(+Player, +Row, +Col, +Slot, +OldPieces, -NewPieces)
% Places an unplaced piece for the specified player on the board.
place_piece(Player, Row, Col, Slot, OldPieces, NewPieces) :-
    select(piece(Player, none, none, none), OldPieces, RemainingPieces), % Select an unplaced piece
    NewPiece = piece(Player, Row, Col, Slot),                            % Create the new piece
    NewPieces = [NewPiece | RemainingPieces].    

% Handle the case where there are no unplaced pieces for the player
place_piece(Player, _, _, _, OldPieces, OldPieces) :-
    format('No unplaced pieces available for player ~w.~n', [Player]), fail.

/* MOVE A PLACED PIECE */
    
move_piece(
    game_state(Board, Pieces, Player, move, MovesLeft),
    piece(Player, Row, Col, Pos),
    Direction,
    game_state(Board, UpdatedPieces, Player, move, NewMovesLeft)
) :-
    % Validate the piece is already on the board
    piece_on_board(Row, Col),

    % Validate the move direction and calculate the new configuration of the piece
    valid_move_direction(Board, Pieces, Player, Row, Col, Pos, Direction, NewRow, NewCol, NewPos),

    % Replace the moved piece in the list of all pieces
    replace_moved_pieces(Pieces, [piece(Player, NewRow, NewCol, NewPos)], UpdatedPieces),

    % Decrement moves left
    NewMovesLeft is MovesLeft - 1.


% valid_move_direction(+Board, +Pieces, +Player, +Row, +Col, +Pos, +Direction, -NewRow, -NewCol, -NewPos)
% Validates the move direction and calculates the new position (disk and slot).
valid_move_direction(Board, Pieces, Player, Row, Col, Pos, right, Row, NewCol, NewPos) :-
    % Stay in the same disk (rightward within the disk)
    NewCol = Col,
    adjacent_position(Pos, right, NewPos),
    valid_square(Board, Player, Row, NewCol, NewPos, Pieces).
valid_move_direction(Board, Pieces, Player, Row, Col, Pos, right, Row, NewCol, NewPos) :-
    % Move to the right disk in the next column
    NewCol is Col + 1,
    adjacent_position(Pos, right, NewPos),
    valid_square(Board, Player, Row, NewCol, NewPos, Pieces).
valid_move_direction(Board, Pieces, Player, Row, Col, Pos, left, Row, NewCol, NewPos) :-
    % Move left within the disk or to the left disk
    NewCol is max(1, Col - 1),
    adjacent_position(Pos, left, NewPos),
    valid_square(Board, Player, Row, NewCol, NewPos, Pieces).
valid_move_direction(Board, Pieces, Player, Row, Col, Pos, up, NewRow, Col, NewPos) :-
    % Move up within the same column
    NewRow is Row + 1,
    adjacent_position(Pos, up, NewPos),
    valid_square(Board, Player, NewRow, Col, NewPos, Pieces).
valid_move_direction(Board, Pieces, Player, Row, Col, Pos, down, NewRow, Col, NewPos) :-
    % Move down within the same column
    NewRow is Row - 1,
    adjacent_position(Pos, down, NewPos),
    valid_square(Board, Player, NewRow, Col, NewPos, Pieces).

% adjacent_position(+CurrentPos, +Direction, -NewPos)
% Determines the new position on the disk based on the direction.
adjacent_position(topLeft, right, topRight).
adjacent_position(topRight, right, topLeft). % When moving to a new disk
adjacent_position(bottomLeft, right, bottomRight).
adjacent_position(bottomRight, right, bottomLeft). % When moving to a new disk
adjacent_position(topLeft, left, topRight). % When moving to the left disk
adjacent_position(topRight, left, topLeft).
adjacent_position(bottomLeft, left, bottomRight).
adjacent_position(bottomRight, left, bottomLeft).
adjacent_position(topLeft, up, bottomLeft).
adjacent_position(bottomLeft, up, bottomRight).
adjacent_position(topRight, up, topLeft).
adjacent_position(bottomRight, up, topRight).
adjacent_position(bottomLeft, down, topLeft).
adjacent_position(bottomRight, down, topRight).


% Replace moved pieces in the full list of all pieces
replace_moved_pieces(AllPieces, MovedPieces, UpdatedPieces) :-
    exclude(is_moved_piece(MovedPieces), AllPieces, NonMovedPieces),
    append(MovedPieces, NonMovedPieces, UpdatedPieces).

% Check if a piece belongs to the list of rotated pieces
is_moved_piece(MovedPieces, piece(Player, Row, Col, _)) :-
    member(piece(Player, Row, Col, _), MovedPieces).

