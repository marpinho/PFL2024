
% Execute move
move(game_state(Board, Pieces, CurrentPlayer, rotate, MovesLeft), move(rotation, Index), game_state(NewBoard, NewPieces, CurrentPlayer, move, 3)) :-
    rotate_row(Board, Index, NewBoard, OldPieces, NewPieces).
move(game_state(Board, Pieces, CurrentPlayer, move, MovesLeft), move(movement, Piece, Path), game_state(Board, NewPieces, CurrentPlayer, move, NewMovesLeft)) :-
    rotate_column(Board, Index, NewBoard, OldPieces, NewPieces).


/* ROTATION */

% Rotate a row or column and update the game state
rotate_row(Board, Index, NewBoard, OldPieces, NewPieces) :-
    nth1(Index, Board, Row),
    maplist(update_disk_and_pieces, Row, RotatedRow, OldPieces, NewPieces),
    replace_row(Board, Index, RotatedRow, NewBoard).

% Rotate a column and update all the pieces affected by the rotation
rotate_column(Board, Index, NewBoard, OldPieces, NewPieces) :-
    transpose(Board, TransposedBoard),
    rotate_row(TransposedBoard, Index, RotatedTransposedBoard, OldPieces, UpdatedPieces),
    transpose(RotatedTransposedBoard, NewBoard). 

% Rotate a single disk and update the positions of pieces present on it.
update_disk_and_pieces(OldDisk, RotatedDisk, OldPieces, UpdatedPieces) :-
    rotate_disk(OldDisk, RotatedDisk),
    update_pieces_on_disk(OldDisk, RotatedDisk, OldPieces, UpdatedPieces).


rotate_disk([TL, TR, BL, BR], [BL, TL, BR, TR]).

update_pieces_on_disk(OldDisk, RotatedDisk, OldPieces, UpdatedPieces) :-
    findall(
        piece(Player, Row, Col, NewSlot), % Update piece record with the new slot.
        (
            member(piece(Player, Row, Col, OldSlot), OldPieces),
            nth1(OldSlot, OldDisk, _),        % Find its original position
            nth1(NewSlot, RotatedDisk, _),    % Find its new position on the rotated disk
            OldSlot \= null                   % Garantee its not placed on the board
        ),
        UpdatedPieces
    ).

replace_row(Board, Index, NewRow, NewBoard) :-
    nth1(Index, Board, _, Rest),
    nth1(Index, NewBoard, NewRow, Rest).

/* PLACE A NEW PIECE */

% Place a new piece on the board
place_piece(Player, Row, Col, Pos, Pieces, [piece(Player, Row, Col, Pos) | Pieces]).

/* MOVE A PLACED PIECE */

move_piece(game_state(Board, Pieces, Player, move, MovesLeft), piece(Player, Row, Col, Pos), [exit], game_state(Board, NewPieces, Player, move, NewMovesLeft)) :-
    valid_exit(Board, Player, Row, Col, Pos),
    mark_piece_off_board(Pieces, piece(Player, Row, Col, Pos), NewPieces),
    NewMovesLeft is MovesLeft - 1.
move_piece(game_state(Board, Pieces, Player, move, MovesLeft), piece(Player, Row, Col, Pos), [NextPos | Path], game_state(Board, NewPieces, Player, move, NewMovesLeft)) :-
    update_piece_position(Pieces, piece(Player, Row, Col, Pos), NextPos, UpdatedPieces),
    move_piece(game_state(Board, UpdatedPieces, Player, move, MovesLeft - 1), piece(Player, Row, Col, NextPos), Path, game_state(Board, NewPieces, Player, move, NewMovesLeft)).
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
