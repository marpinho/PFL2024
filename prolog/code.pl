% Sample game state
sample_game_state(game_state(
  [
    [disc(neutral, player1Exclusive, player2Exclusive, inaccessible), disc(player1Exclusive, neutral, inaccessible, player2Exclusive), disc(player2Exclusive, inaccessible, neutral, player1Exclusive), disc(inaccessible, player2Exclusive, player1Exclusive, neutral)],
    [disc(player1Exclusive, neutral, inaccessible, player2Exclusive), disc(inaccessible, player2Exclusive, neutral, player1Exclusive), disc(player2Exclusive, player1Exclusive, inaccessible, neutral), disc(neutral, inaccessible, player2Exclusive, player1Exclusive)],
    [disc(player2Exclusive, inaccessible, neutral, player1Exclusive), disc(neutral, player1Exclusive, player2Exclusive, inaccessible), disc(inaccessible, neutral, player1Exclusive, player2Exclusive), disc(player1Exclusive, player2Exclusive, neutral, inaccessible)],
    [disc(inaccessible, player2Exclusive, player1Exclusive, neutral), disc(player2Exclusive, inaccessible, neutral, player1Exclusive), disc(neutral, player1Exclusive, player2Exclusive, inaccessible), disc(player1Exclusive, neutral, inaccessible, player2Exclusive)]
  ],
  [
    piece(player1, 1, 2, topLeft),       % Placed on the board
    piece(player1, none, none, none),    % Unplaced piece
    piece(player1, none, none, none),    % Unplaced piece
    piece(player1, 2, 3, bottomRight),   % Placed on the board
    piece(player2, 3, 4, topRight),      % Placed on the board
    piece(player2, none, none, none)     % Unplaced piece
  ],
  player1,  % Current player
  rotate,   % Current phase (rotation or movement)
  1         % Remaining moves (1 when it is time to rotate)
)).

% 1- Display - output only

% 1.1- Main Functions
% The game display prints the board and a simple caption. The board is printed row by row, recursively.

% display_game(+GameState)
% The game display prints the board and a simple caption. The board is printed row by row, recursively.
display_game(game_state(Board, Pieces, CurrentPlayer, CurrentPhase, RemainingMoves)) :-
    nl,
    format('Current Player: ~w~n', [CurrentPlayer]),  % Display the current player
    format('Current Phase: ~w~n', [CurrentPhase]),  % Display the current phase (rotate or move)
    format('Remaining Moves: ~d~n', [RemainingMoves]),  % Display the number of remaining moves
    nl, display_board(Board, Pieces, 1),
    write('+------+------+------+------+------+------+------+------+'), nl, % Board
    display_unplaced_pieces(Pieces),  % Print unplaced pieces after the board
    nl, write(' N  - Neutral Square'), nl, write(' X  - Inaccessible Square'), nl,
    write(' E1 - Player 1 exclusive Square'), nl, write(' E2 - Player 2 exclusive Square'), nl,
    write(' P1 - Player 1 piece'), nl, write(' P2 - Player 2 piece'), nl, nl.

% display_unplaced_pieces(+Pieces)
display_unplaced_pieces(Pieces) :-
    count_unplaced_pieces(Pieces, player1, Player1Count),
    count_unplaced_pieces(Pieces, player2, Player2Count),
    format(' Unplaced Player 1 pieces: ~d~n', [Player1Count]),
    format(' Unplaced Player 2 pieces: ~d~n', [Player2Count]).
    
% count_unplaced_pieces(+Pieces, +Player, -Count)
count_unplaced_pieces(Pieces, Player, Count) :-
    findall(Player, member(piece(Player, none, none, none), Pieces), UnplacedPieces),
    length(UnplacedPieces, Count).

% 1.2- Line Logic
% Each line in the board is printed recursively.

% display_board(+Board, +Pieces, +RowIndex)
display_board([], _, _) :- % Base Case: all rows printed (empty board)
    !.
display_board([Row|Board], Pieces, RowIndex) :- % Recursive Step: print row, print board excluding row
    write('+------+------+------+------+------+------+------+------+'), nl,
    display_disc_row(Row, Pieces, RowIndex, 1, top), 
    display_disc_row(Row, Pieces, RowIndex, 1, bottom), % Each row occupies two lines in the output, that must be printed separately
    NextRow is RowIndex + 1, display_board(Board, Pieces, NextRow).

% display_disc_row(+Row, +Pieces, +RowIndex, +ColIndex, +Half)
display_disc_row([], _, _, _, _) :- % Base Case: no more squares, move to next line/row
    write('|'), nl.
display_disc_row([Disc|Rest], Pieces, RowIndex, ColIndex, Half) :- % Recursive Step: print the top or bottom half of a disk
    write('| '),
    print_disc_section(Disc, Pieces, RowIndex, ColIndex, Half),  % Since we need to diferentiate between the Top and the Bottom sections, we need to encapsulate the printing itself in a new predicate
    NewColIndex is ColIndex + 1, display_disc_row(Rest, Pieces, RowIndex, NewColIndex, Half).

% print_disc_section(+Disc, +Pieces, +Row, +Col, +Half)
print_disc_section(disc(TL, TR, _, _), Pieces, Row, Col, top) :- % Top-Half of the disk
    format_disc_square(TL, Row, Col, topLeft, Pieces, Left), 
    format_disc_square(TR, Row, Col, topRight, Pieces, Right),
    format('~w ~w ', [Left, Right]).
print_disc_section(disc(_, _, BL, BR), Pieces, Row, Col, bottom) :- % Bottom-Half of the disk
    format_disc_square(BL, Row, Col, bottomLeft, Pieces, Left),
    format_disc_square(BR, Row, Col, bottomRight, Pieces, Right),
    format('~w ~w ', [Left, Right]).  % Print the formatted values.

% format_disc_square(+Type, +Row, +Col, +Square, +Pieces, -Output)
format_disc_square(Type, Row, Col, Square, Pieces, Output) :- % Identifies the correct string to print
    format_disc_square_piece(Row, Col, Square, Pieces, Output); % If a piece is in the square it will take precedence
    format_disc_square_blossom(Type, Row, Col, Square, Pieces, Output).

% format_disc_square_piece(+Row, +Col, +Square, +Pieces, -Output)
format_disc_square_piece(Row, Col, Square, Pieces, Output) :-
    piece_on_square(Pieces, Row, Col, Square, Player),
    Player \= '',  % The empty string indicates that the piece isn't present
    piece_representation(Player, Output).

% format_disc_square_blossom(+Type, +Row, +Col, +Square, +Pieces, -Output)
format_disc_square_blossom(Type, _, _, _, Pieces, Output) :-
    piece_on_square(Pieces, _, _, _, ''),  % Pieces take precedence
    square_representation(Type, Output).

% piece_on_square(+Pieces, +Row, +Col, +Square, -Player)
piece_on_square(Pieces, Row, Col, Square, Player) :-
    member(piece(Player, Row, Col, Square), Pieces),
    !.  % Stop after finding the first match.
piece_on_square(_, _, _, _, '').  % Default case if no piece is found.


% 1.3- Square Representations

% square_representation(+BlossomType, -Representation)
square_representation(neutral, '  N  ').
square_representation(inaccessible, '  X  ').
square_representation(player1Exclusive, ' E-1 ').
square_representation(player2Exclusive, ' E-2 ').

% piece_representation(+PlayerPiece, -Representation)
piece_representation(player1, ' P-1 ').
piece_representation(player2, ' P-2 ').
piece_representation('', '     ').  % Empty square


% 2- Movement

% 2.1- Main Function
% move(+GameState, +Move, -NewGameState)
move(GameState, Move, NewGameState) :-
    valid_moves(GameState, ValidMoves),  % Get the list of valid moves.
    ( member(Move, ValidMoves) ->  % Check if the Move is valid.
        perform_rotation_move(GameState, Move, NewGameState)  % Perform the move if valid.
    ; NewGameState = game_state([], [], _, _, _)  % Invalid move: return empty game state.
    ).

% perform_rotation_move(+GameState, +Move, -NewGameState)
% Performs a rotation move and updates the game state.
perform_rotation_move(game_state(Board, Pieces, CurrentPlayer, rotate, _), move(rotation, row(RowIndex)), game_state(NewBoard, Pieces, CurrentPlayer, movePiece, 3)) :-
    rotate_row(Board, RowIndex, NewBoard).  % Rotate the specified row and update phase.
perform_rotation_move(game_state(Board, Pieces, CurrentPlayer, rotate, _), move(rotation, col(ColIndex)), game_state(NewBoard, Pieces, CurrentPlayer, movePiece, 3)) :-
    rotate_column(Board, ColIndex, NewBoard).  % Rotate the specified column and update phase.



% 2.2 - Validation
% Players can either rotate a row or col or move a piece in each move. This depends on the board configuration and on the next move type.

% valid_moves(+GameState, -ListOfMoves)
% Determines the list of valid moves based on the current phase.
valid_moves(game_state(Board, Pieces, CurrentPlayer, CurrentPhase, _RemainingMoves), ListOfMoves) :-
    valid_rotation_moves(CurrentPhase, RotationMoves),
    valid_piece_moves(CurrentPhase, Board, Pieces, CurrentPlayer, PieceMoves),  % Add piece moves.
    append(RotationMoves, PieceMoves, ListOfMoves).  % Combine lists.

% valid_rotation_moves(+CurrentPhase, -RotationMoves)
valid_rotation_moves(rotate, RotationMoves) :- % Rotation mode: return the 16 possible rotations.
    findall(move(rotation, row(RowIndex)), between(1, 4, RowIndex), RowMoves),  % All row rotations.
    findall(move(rotation, col(ColIndex)), between(1, 4, ColIndex), ColMoves),  % All column rotations.
    append(RowMoves, ColMoves, RotationMoves).
valid_rotation_moves(movePiece, []). % No rotations allowed during the movePiece phase.

% valid_piece_moves(+CurrentPhase, +Board, +Pieces, +CurrentPlayer, -PieceMoves)
valid_piece_moves(movePiece, Board, Pieces, CurrentPlayer, PieceMoves) :-
    findall(move(movement, (Row, Col), Direction), valid_piece_move(Board, Pieces, CurrentPlayer, (Row, Col), Direction), PieceMoves).
valid_piece_moves(rotate, _, _, _, []).  % No piece moves allowed during the rotate phase.

valid_piece_move(Board, Pieces, CurrentPlayer, (Row, Col), Direction) :-
    member(piece(CurrentPlayer, Row, Col, Section), Pieces),  % Get the player’s piece.
    Row \= none, Col \= none,  % Ensure it's placed on the board.
    adjacent_position(Section, Row, Col, Direction, NewSection, NewRow, NewCol),  % Get new section/position.
    NewSection \= none,  % Ensure the new section exists (no "up" from topLeft, etc.).
    valid_square(Board, Pieces, CurrentPlayer, NewRow, NewCol, NewSection).  % Check the target square.

% adjacent_position(+Section, +Row, +Col, +Direction, -NewSection, -NewRow, -NewCol)
% Handles transitions within the same disc or to adjacent discs based on direction.
adjacent_position(topLeft, Row, Col, right, topRight, Row, Col).  % Move to topRight within the same disc.
adjacent_position(topLeft, Row, Col, left, topRight, Row, NewCol) :-  % Move to left disc’s topRight.
    NewCol is Col - 1, NewCol > 0.
adjacent_position(topLeft, Row, Col, up, none, _, _).  % No valid move upwards from topLeft.
adjacent_position(topLeft, Row, Col, down, bottomRight, Row, Col).  % Move to bottomRight within the same disc.

adjacent_position(topRight, Row, Col, right, topLeft, Row, NewCol) :-  % Move to right disc’s topLeft.
    NewCol is Col + 1, NewCol =< 4.
adjacent_position(topRight, Row, Col, left, topLeft, Row, Col).  % Move to topLeft within the same disc.
adjacent_position(topRight, Row, Col, up, none, _, _).  % No valid move upwards from topRight.
adjacent_position(topRight, Row, Col, down, bottomLeft, Row, Col).  % Move to bottomLeft within the same disc.

adjacent_position(bottomLeft, Row, Col, right, bottomRight, Row, Col).  % Move to bottomRight within the same disc.
adjacent_position(bottomLeft, Row, Col, left, bottomRight, Row, NewCol) :-  % Move to left disc’s bottomRight.
    NewCol is Col - 1, NewCol > 0.
adjacent_position(bottomLeft, Row, Col, up, topRight, Row, Col).  % Move to topRight within the same disc.
adjacent_position(bottomLeft, Row, Col, down, none, _, _).  % No valid move downwards from bottomLeft.

adjacent_position(bottomRight, Row, Col, right, bottomLeft, Row, NewCol) :-  % Move to right disc’s bottomLeft.
    NewCol is Col + 1, NewCol =< 4.
adjacent_position(bottomRight, Row, Col, left, bottomLeft, Row, Col).  % Move to bottomLeft within the same disc.
adjacent_position(bottomRight, Row, Col, up, topLeft, Row, Col).  % Move to topLeft within the same disc.
adjacent_position(bottomRight, Row, Col, down, none, _, _).  % No valid move downwards from bottomRight.


% valid_square(+Board, +Pieces, +Player, +Row, +Col, +Section)
valid_square(Board, Pieces, Player, Row, Col, Section) :-
    nth1(Row, Board, RowList),  % Get the row.
    nth1(Col, RowList, disc(TL, TR, BL, BR)),  % Get the disc and its sections.
    section_blossom_type(Section, TL, TR, BL, BR, BlossomType),  % Get the blossom type for the section.
    valid_bl_type(Player, BlossomType),  % Check if the blossom type is valid for the player.
    piece_on_square(Pieces, Row, Col, Section, '').  % Ensure no piece occupies this square.

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



% 2.X - Rotation
% Disks are always rotated 90º clockwise, Rows or Cols only matter in defining the list of disks to rotate

% rotate_disks(+DiscList, -RotatedDiscList)
% The main function, responsible for rotating a list of disks
rotate_disks([], []). % Base Case: No more disks to rotate
rotate_disks([disc(TL, TR, BL, BR) | Rest], [disc(BL, TL, BR, TR) | RotatedRest]) :- % Recursive Step: Rotate clockwise
    rotate_disks(Rest, RotatedRest).

% replace_in_row(+List, +Index, +NewElement, -NewList)
replace_in_row(List, Index, NewElement, NewList) :- % Replace a row in the board
    append(Prefix, [_ | Suffix], List),
    append(Prefix, [NewElement | Suffix], NewList),
    length(Prefix, Index1),
    Index is Index1 + 1.

% rotate_row(+Board, +RowIndex, -NewBoard)
rotate_row(Board, RowIndex, NewBoard) :- % Very straightforward: read disks in row and replace in board
    nth1(RowIndex, Board, Row),
    rotate_disks(Row, NewRow),
    replace_in_row(Board, RowIndex, NewRow, NewBoard).

% rotate_column(+Board, +ColIndex, -NewBoard)
rotate_column(Board, ColIndex, NewBoard) :- % Follows the same strategy as rotate_row, although not as direct. The disks in the col must be identified with a recursive function. All rows will need an update.
    extract_column(Board, ColIndex, Column),  % Get the specified column.
    rotate_disks(Column, RotatedColumn),  % Rotate each disc in the column.
    replace_column(Board, ColIndex, RotatedColumn, NewBoard).

% extract_column(+Board, +ColIndex, -Column)
extract_column([], _, []). % Base Case: All rows processed
extract_column([Row | Rest], ColIndex, [Disc | ColumnRest]) :- % Recursive Step: Extract the disk in the col index from a row.
    nth1(ColIndex, Row, Disc),
    extract_column(Rest, ColIndex, ColumnRest).

% replace_column(+Board, +ColIndex, +NewColumn, -NewBoard)
% Recursively updates one disk in each row
replace_column([], _, [], []). % Base Case: All rows processed
replace_column([Row | RestBoard], ColIndex, [NewDisc | RestColumn], [NewRow | RestNewBoard]) :- % Recursive Step: Update one disk in a row.
    replace_in_row(Row, ColIndex, NewDisc, NewRow),
    replace_column(RestBoard, ColIndex, RestColumn, RestNewBoard).


:- initialization(main).

main :-
    sample_game_state(GameState),
    % Initial state display
    write('--- Initial Game State ---'), nl,
    display_game(GameState),
    
    % Perform a rotation move
    write('--- After Rotation Move ---'), nl,
    move(GameState, move(rotation, row(2)), NewGameState),
    display_game(NewGameState),
    
    % Display valid piece moves after rotation
    write('--- Valid Piece Moves Debug ---'), nl,
    valid_moves(NewGameState, Moves),
    format('Valid Moves: ~w~n', [Moves]),
    halt.