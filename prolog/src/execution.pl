:- use_module(library(random)).
:- use_module(library(lists)).
:- [validation].


/*PROMPT THE PLAYER*/

% prompt_game_mode(-GameMode)
% Prompt the player to choose the game mode
prompt_game_mode(GameMode) :-
	write('Enter the game mode: '),nl,
	write('(1) Player vs. Player'),nl,
	write('(2) Player vs. AI'),nl,
	write('(3) AI vs. AI'),nl,
	write('Enter (0) to quit the Game'), nl,  
    read(GameMode),
    between(0, 3, GameMode). 

% prompt_ai_level(-AILevel)
% Prompt the player to choose the AI level
prompt_ai_level(AILevel) :-
    write('Enter the AI level: 1 or 2'),nl,
    read(AILevel),
    between(1, 2, AILevel).

% prompt_rotation(-Type, -Index)
% Prompt the player to choose a row or colum to rotate
prompt_rotation(RotationType, Index):-
    write('Enter (1) to rotate a row or (2) to rotate a column:'), nl,
    read(Rotation),
    between(1, 2, Rotation),
    rotation_representation(Rotation, RotationType),
    prompt_rotation_index(Index).

% prompt_rotation_index(-Index)
% Prompt the player to choose the Index of the row or column to rotate
prompt_rotation_index(Index) :-
    write('Enter its number (1-4): '), nl,
    read(Index),
    between(1, 4, Index).
    
% prompt_movement(-MovementType)
% Prompt for movement actions
prompt_movement_type(MovementType) :-
    write('Enter (1) to place a new piece or (2) to move one:'), nl,
    read(Movement),
    move_representation(Movement, MovementType).

% prompt_basic_movement_config(-RowIndex, -ColIndex, -PosInDisc)
% Prompt for basic movement configuration
prompt_basic_movement_config(RowIndex, ColIndex, PosInDisc) :-
    write('Enter the disc\'s row : '), nl,
    read(RowIndex),
    between(1, 4, RowIndex),
    write('Enter the disc\'s column'), nl,
    read(ColIndex),
    between(1, 4, ColIndex),
    write('Enter the position on the disc: '), nl,
    write('(1) Top left'), nl,
    write('(2) Top right'), nl,
    write('(3) Bottom left'), nl,    
    write('(4) Bottom right'), nl,
    read(Position),
    between(1, 4, Position),
    position_representation(Position,PosInDisc).

% prompt_movement_direction(-Direction)	
% Prompt for movement direction
prompt_movement_direction(Direction) :-
    write('Enter the direction to move the piece: '), nl,
    write('(1) Up'), nl,
    write('(2) Down'), nl,
    write('(3) Left'), nl,
    write('(4) Right'), nl,
    read(DirectionNumber),
    between(1, 4, DirectionNumber),
    direction_representation(DirectionNumber, Direction).

% mappings for rotation types
rotation_representation(1, rotate_row).
rotation_representation(2, rotate_col).

% mappings for movement types
move_representation(1, place_piece).
move_representation(2, move_piece).

% mappings for position types
position_representation(1, topLeft).
position_representation(2, topRight).
position_representation(3, bottomLeft).
position_representation(4, bottomRight).

% mappings for direction types
direction_representation(1, up).
direction_representation(2, down).
direction_representation(3, left).
direction_representation(4, right).


/* GENERATE BOARD*/

% generate_board(-Board)
% Generate a 4x4 board with randomized discs
generate_board(Board) :-
    length(FlatBoard, 16),  % Create a list of 16 discs
    maplist(random_disc, FlatBoard),  % Generate 16 random discs
    partition(FlatBoard, 4, Board).

% random_disc(-Disc)
% Randomly generate a disc with one of each Type in random order
random_disc(disc(Type1, Type2, Type3, Type4)) :-
    random_permutation([e1, e2, n, x], [Type1, Type2, Type3, Type4]).

% partition(+List, +Size, -Partitioned)
% Partition a flat list into a 4x4 board
partition([], _, []).
partition(List, Size, [Row|Rest]) :-
    length(Row, Size),
    append(Row, Tail, List),
    partition(Tail, Size, Rest).

/* GENERATE PIECES*/

% initialize_pieces(+Player1, +Player2, -Pieces)
% Initialize player pieces 
initialize_pieces(Player1, Player2, Pieces) :-
    format('Player 1: ~w~n', [Player1]),
    format('Player 2: ~w~n', [Player2]),
    initialize_player_pieces(Player1, P1Pieces),
    initialize_player_pieces(Player2, P2Pieces),
    append(P1Pieces, P2Pieces, Pieces).

% initialize_player_pieces(+Player, -Pieces)
initialize_player_pieces(Player, Pieces) :-
    length(Pieces, 5),
    maplist(=(piece(Player, none, none, none)), Pieces).


/* ROTATION */

% rotate_row(+Board, +OldPieces, +RowIndex, -NewBoard, -UpdatedPieces)
% Rotate a row and update all
rotate_row(Board, OldPieces, RowIndex, NewBoard, UpdatedPieces) :-
    nth1(RowIndex, Board, Row),
    rotate_all_discs(Row, RotatedRow),
    replace_row(Board, RowIndex, RotatedRow, NewBoard),
    pieces_in_row(OldPieces, RowIndex, PiecesInRow),
    update_pieces_after_rotation(PiecesInRow, OldPieces, UpdatedPieces). % Update pieces declaratively


% rotate_column(+Board, +OldPieces, +ColIndex, -NewBoard, -UpdatedPieces)
% Rotate a column and update all 
rotate_column(Board, OldPieces, ColIndex, NewBoard, UpdatedPieces) :-
    transpose(Board, TransposedBoard),                                              % Transpose to treat column as row
    nth1(ColIndex, TransposedBoard, Column),                                        % Get the target column
    rotate_all_discs(Column, RotatedColumn),                                        % Rotate all discs in the column
    replace_row(TransposedBoard, ColIndex, RotatedColumn, RotatedTransposedBoard),  % Replace the rotated column
    transpose(RotatedTransposedBoard, NewBoard),                                    % Transpose back to restore board orientation
    pieces_in_column(OldPieces, ColIndex, PiecesInColumn),                          % Extract pieces in the target column
    update_pieces_after_rotation(PiecesInColumn, OldPieces, UpdatedPieces).         % Update pieces declaratively

% update_pieces_after_rotation(+AffectedPieces, +OldPieces, -UpdatedPieces)
% Updates pieces affected by rotation declaratively.
update_pieces_after_rotation([], OldPieces, OldPieces).  % No affected pieces, no change.
update_pieces_after_rotation(AffectedPieces, OldPieces, UpdatedPieces) :-
    rotate_pieces_90_clockwise(AffectedPieces, RotatedPieces), % Rotate the affected pieces
    replace_rotated_pieces(OldPieces, RotatedPieces, UpdatedPieces). % Replace them in the full list.

% rotate_all_discs(+Discs, -RotatedDiscs)
% Rotates all discs in the input list.
rotate_all_discs([], []).                       % Base case: An empty list produces an empty list.
rotate_all_discs([Disc | Rest], [RotatedDisc | RotatedRest]) :-
    rotate_disc(Disc, RotatedDisc),             % Rotate the current disc
    rotate_all_discs(Rest, RotatedRest).        % Recursively rotate the rest of the list.

% rotate_disc(+OldDisc, -NewDisc)
% Rotates a single disc clockwise.
rotate_disc(disc(TL, TR, BL, BR), disc(BL, TL, BR, TR)).

% pieces_in_row(+Pieces, +RowIndex, -PiecesInRow)
% Retrieves all pieces located in the specified row.
pieces_in_row(Pieces, RowIndex, PiecesInRow) :-
    findall(
        piece(Player, RowIndex, ColIndex, Type),
        member(piece(Player, RowIndex, ColIndex, Type), Pieces),
        PiecesInRow
    ).

% pieces_in_column(+Pieces, +ColIndex, -PiecesInColumn)
% Retrieves all pieces located in the specified column.
pieces_in_column(Pieces, ColIndex, PiecesInColumn) :-
    findall(
        piece(Player, RowIndex, ColIndex, Position),
        member(piece(Player, RowIndex, ColIndex, Position), Pieces),
        PiecesInColumn
    ).

% rotate_pieces_90_clockwise(+Pieces, -RotatedPieces)
% Rotates all pieces in the input list 90 degrees clockwise using maplist.
rotate_pieces_90_clockwise(Pieces, RotatedPieces) :-
    maplist(rotate_piece_90_clockwise, Pieces, RotatedPieces).

% rotate_piece_with_coordinates(+Piece, -RotatedPiece)
% Rotate a single piece.
rotate_piece_90_clockwise(piece(Player, RowIndex, ColIndex, OldPos), piece(Player, RowIndex, ColIndex, NewPos)) :-
    get_90_clockwise_pos(OldPos, NewPos).

% get_90_clockwise_pos(+OldPos, -NewPos)
% a single pieces position 90 degrees clockwise.
get_90_clockwise_pos(topLeft, topRight).
get_90_clockwise_pos(topRight, bottomRight).
get_90_clockwise_pos(bottomLeft, topRight).
get_90_clockwise_pos(bottomRight, bottomLeft).

replace_row(Board, IndexRow, NewRow, NewBoard) :-
    nth1(IndexRow, Board, _, Rest),
    nth1(IndexRow, NewBoard, NewRow, Rest).

% Replace rotated pieces in the full list of all pieces
replace_rotated_pieces(AllPieces, RotatedPieces, UpdatedPieces) :-
    exclude(is_rotated_piece(RotatedPieces), AllPieces, NonRotatedPieces),
    append(RotatedPieces, NonRotatedPieces, UpdatedPieces).

% Check if a piece belongs to the list of rotated pieces
is_rotated_piece(RotatedPieces, piece(Player, RowIndex, ColIndex, _)) :-
    member(piece(Player, RowIndex, ColIndex, _), RotatedPieces).

% valid_rotation_moves(-RotationMoves)
% Determines the list of valid rotation moves.
valid_rotation_moves(RotationMoves) :-
    findall( move(rotate_row, RowIndex), between(1, 4, RowIndex), RowMoves),  % All row rotations.
    findall( move(rotate_col, ColIndex), between(1, 4, ColIndex), ColMoves),  % All row rotations.
    append(RowMoves, ColMoves, RotationMoves).

/* PLACE A NEW PIECE */

% Place an unplaced piece for the specified player on the board.
% place_piece(+Board, +Pieces, +Player, +RowIndex, +ColIndex, +PosInDisc, -UpdatedPieces)
place_piece(_, Pieces, Player, RowIndex, ColIndex, PosInDisc, UpdatedPieces) :-                                                                                 % Check if there are pieces left
    separate_piece(Player, none, none, none, Pieces, RemainingPieces),                          % Get the piece to place
    append([piece(Player,RowIndex, ColIndex, PosInDisc)], RemainingPieces, UpdatedPieces).      % Update the pieces list   

% valid_piece_placements(+Board, +Pieces, +Player, -PiecePlacementMoves)
% Finds all valid piece placement moves for the player.\

valid_piece_placements(Board, Pieces, Player, PiecePlacementMoves) :-
    validate_has_unplaced_pieces(Pieces, Player),
    player_starting_requirements(Player, RowIndex, Positions),
    findall(
        move(place_piece, RowIndex, ColIndex, PosInDisc),
        (
            between(1, 4, ColIndex),
            member(PosInDisc, Positions),
            validate_position(Player, Board, Pieces, RowIndex, ColIndex, PosInDisc)                   
        ),
        PiecePlacementMoves
    ).


/* MOVE A PIECE */

% move_piece(+Board, +Pieces, +Player, +RowIndex, +ColIndex, +PosInDisc, +Direction, -UpdatedPieces)
% Move a piece and update the list of pieces.
move_piece(Board, Pieces, Player, RowIndex, ColIndex, PosInDisc, Direction, UpdatedPieces) :-                                                                                           % Check if there are pieces left
    separate_piece(Player, RowIndex, ColIndex, PosInDisc, Pieces, RemainingPieces),                        % Get the piece to move
    validate_piece_move(Board, Pieces, Player, RowIndex, ColIndex, PosInDisc, Direction, UpdatedPiece),    % Validate the move
    append([UpdatedPiece], RemainingPieces, UpdatedPieces).                                                % Update the pieces list

separate_piece(Player, RowIndex, ColIndex, PosInDisc, Pieces, RemainingPieces) :-
    select(piece(Player, RowIndex, ColIndex, PosInDisc), Pieces, RemainingPieces).      

% valid_piece_moves(+Board, +Pieces, +Player, -PieceMoves)
% Determines the list of all possible valid piece moves for the player.
valid_piece_moves(Board, Pieces, Player, PieceMoves) :-
    findall(
        move(move_piece, RowIndex, ColIndex, PosInDisc, Direction),
        (
            member(piece(Player, RowIndex, ColIndex, PosInDisc), Pieces),
            validate_piece_move(Board, Pieces, Player, RowIndex, ColIndex, PosInDisc, Direction, _)
        ),
        PieceMoves
    ).



/* ALL MOVES */

% valid_moves(+GameState, -ListOfMoves)
% Determines the list of valid moves for the player.

% Handles the rotation phase when the player has all 4 moves available.
valid_moves( game_state(_, _, _, _, 4), ListOfMoves):-
    valid_rotation_moves(ListOfMoves).

% Handles the placement/movement phase when the player has 3 or fewer moves left.
valid_moves( game_state(Board, Pieces, Player, _, NumberOfMoves), ListOfMoves):-
    between(1, 3, NumberOfMoves),
    valid_piece_placements(Board, Pieces, Player, PiecePlacementMoves),
    valid_piece_moves(Board, Pieces, Player, PieceMoves),
    append(PieceMoves, PiecePlacementMoves, ListOfMoves).

% simulate_move(+GameState, +Move, -NewGameState)
% Simulates the execution of a move to predict the resulting game state.
simulate_move(GameState, Move, NewGameState) :-
    move(GameState, Move, NewGameState).

% value(+GameState, +Player, -Value)
% Determines the value of the game state for the specified player.
value(game_state(_, Pieces, Player, _, _), Player, Value) :-
    findall(PieceValue, (member(piece(Player, Row, _, _), Pieces), piece_value(Row, PieceValue)), Scores),
    sumlist(Scores, Value).

% piece_value(+Row, -Value)
% Assigns a score to a piece based on its row closer to opponents side is better
piece_value(off, 10). % Maximum score for pieces that have crossed the field.
piece_value(none, 0). % Maximum score for pieces that have crossed the field.
piece_value(Row, Value) :-
    integer(Row), Value is Row. % Assign a score based on the row number.


