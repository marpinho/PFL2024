:- use_module(library(between)).


/* GENERAL VALIDATION */

% validate_position( +Player, +Board,  +Pieces, +RowIndex, +ColIndex, +PosInDisc)
% Checks if the given position on the board is valid for the player and unoccupied.

validate_position(Player, Board, Pieces, Row, Col, Pos) :-
    between(1, 4, Row), 
    between(1, 4, Col), 
    validate_accessible(Player, Board, Pieces, Row, Col, Pos).

validate_accessible(Player, Board, Pieces, RowIndex, ColIndex, PosInDisc) :-
    nth1(RowIndex, Board, Row),                                             % Extract the Row
    nth1(ColIndex, Row, Disc),                                              % Extract the Disc at Column
    disc_accessible(PosInDisc, Disc, Player),                               % Check if the Position is accessible
    \+ piece_occupying_position(Pieces, RowIndex, ColIndex, PosInDisc).     % Ensure no piece is occupying the position.

% disc_accessible(+Position, +Disc, +Player)
% Checks if the given position on the disc is accessible to the player.
disc_accessible(topLeft, disc(Type1, _, _, _), Player) :-
    accessible(Type1, Player).
disc_accessible(topRight, disc(_, Type2, _, _), Player) :-
    accessible(Type2, Player).
disc_accessible(bottomLeft, disc(_, _, Type3, _),  Player) :-
    accessible(Type3, Player).
disc_accessible(bottomRight, disc(_, _, _, Type4), Player) :-
    accessible(Type4, Player).

% accessible(+SquareType, +Player)
% Determines if a square type is accessible to the player.
accessible(n, _).             % Neutral squares are accessible to all players.
accessible(e1, player1).      % Exclusive squares for Player1.
accessible(e1, computer1-_).       % Exclusive squares for Player1.
accessible(e2, player2).      % Exclusive squares for Player2.
accessible(e2, computer2-_).       % Exclusive squares for Player2.


% piece_occupying_position(+Pieces, +RowIndex, +ColIndex, +PosInDisc)
% Checks if a piece occupies the specified position.
piece_occupying_position(Pieces, RowIndex, ColIndex, PosInDisc) :-
    member(piece(_, RowIndex, ColIndex, PosInDisc), Pieces).



/* PLACE NEW PIECE */

% validate_has_unplaced_pieces(+Pieces, +Player)
% Checks if the player has unplaced pieces.
validate_has_unplaced_pieces(Pieces, Player) :-
    count_unplaced_pieces(Pieces, Player, Count),
    Count > 0.


% validate_starting_position(+Board, +Pieces, +Player, +RowIndex, +ColIndex, +PosInDisc)
% Validate starting position
validate_starting_position(Board, Pieces, Player, RowIndex, ColIndex, PosInDisc) :-
    player_starting_requirements(Player, Row, Positions), 
    RowIndex = Row,
    member(PosInDisc, Positions),                   
    validate_position(Player, Board, Pieces, RowIndex, ColIndex, PosInDisc).


% player_starting_requirements(+Player, -RowIndex, -PosInDisc)
% Determines the starting requirements for the player.
player_starting_requirements(Player, 1, [topLeft, topRight]) :-
    (Player = player1 ; Player = computer1-_), !.
player_starting_requirements(Player, 4, [bottomLeft, bottomRight]) :-
    (Player = player2 ; Player = computer2-_).


/* MOVE A PIECE */

% validate_has_placed_pieces(+Pieces, +Player)
% Checks if the player has placed any pieces before moving.
validate_has_placed_pieces(Pieces, Player) :-
    count_placed_pieces(Pieces, Player, Count),
    Count > 0.

% validate_piece_exists(+Pieces, +Player, +Row, +Col, +Pos)
validate_piece_exists(Pieces, Player, Row, Col, Pos) :-
    member(piece(Player, Row, Col, Pos), Pieces), !.
validate_piece_exists(_, _, _, _, _) :-
    !, write('The specified piece does not exist!'), nl, fail.

% validate_piece_move(+Board, +Pieces, +Player, +Row, +Col, +Pos, +Direction, -UpdatedPiece)
% Checks if moving a piece in the specified direction is valid  and and creates the new piece.
validate_piece_move(Board, Pieces, Player, Row, Col, Pos, Direction, piece(Player, NewRow, NewCol, NewPos)) :-
    get_new_position(Row, Col, Pos, Direction, NewRow, NewCol, NewPos),  % Calculate the new position
    validate_position(Player, Board, Pieces, NewRow, NewCol, NewPos).       % Validate the new position
validate_piece_move(_, _, Player, _, _, _, _) :-
    (Player = player1 ; Player = player2),
    !, write('You can\'t move the piece in the specified direction!'), nl, fail.

% get_new_position(+RowIndex, +ColIndex, +PosInDisc, +Direction, -NewRow, -NewCol, -NewPos)
% Calculate the new position of a piece after moving it in the specified direction
% get_new_position(+Row, +Col, +Pos, +Direction, -NewRow, -NewCol, -NewPos)
% Determines the new position, row, and column after moving in the specified direction.

% Moving within the same disc (horizontal)
get_new_position(Row, Col, topLeft, right, Row, Col, topRight).
get_new_position(Row, Col, topRight, left, Row, Col, topLeft).
get_new_position(Row, Col, bottomLeft, right, Row, Col, bottomRight).
get_new_position(Row, Col, bottomRight, left, Row, Col, bottomLeft).

% Moving within the same column (vertical)
get_new_position(Row, Col, topLeft, down, Row, Col, bottomLeft).
get_new_position(Row, Col, topRight, down, Row, Col, bottomRight).
get_new_position(Row, Col, bottomLeft, up, Row, Col, topLeft).
get_new_position(Row, Col, bottomRight, up, Row, Col, topRight).

% Moving between discs (horizontal)
get_new_position(Row, Col, topLeft, left, Row, NewCol, topRight) :-
    NewCol is Col - 1, NewCol > 0.
get_new_position(Row, Col, topRight, right, Row, NewCol, topLeft) :-
    NewCol is Col + 1, NewCol =< 4.
get_new_position(Row, Col, bottomLeft, left, Row, NewCol, bottomRight) :-
    NewCol is Col - 1, NewCol > 0.
get_new_position(Row, Col, bottomRight, right, Row, NewCol, bottomLeft) :-
    NewCol is Col + 1, NewCol =< 4.

% Moving between discs (vertical)
get_new_position(Row, Col, topLeft, up, NewRow, Col, bottomLeft) :-
    NewRow is Row - 1, NewRow > 0.
get_new_position(Row, Col, topRight, up, NewRow, Col, bottomRight) :-
    NewRow is Row - 1, NewRow > 0.
get_new_position(Row, Col, bottomLeft, down, NewRow, Col, topLeft) :-
    NewRow is Row + 1, NewRow =< 4.
get_new_position(Row, Col, bottomRight, down, NewRow, Col, topRight) :-
    NewRow is Row + 1, NewRow =< 4.

get_new_position(_, _, _, _, _, _, _) :- fail.

/* UXILIAR PREDICATES */

% count_player_pieces(+Pieces, +Player, -Crossed, -Unplaced)
% Counts the number of crossed and unplaced pieces for the specified player.
count_player_pieces(Pieces, Player, Crossed, Unplaced) :-
    count_crossed_pieces(Pieces, Player, Crossed),
    count_unplaced_pieces(Pieces, Player, Unplaced).

% count_unplaced_pieces(+Pieces, +Player, -Count)
% Counts the number of unplaced pieces for the specified player.
count_unplaced_pieces(Pieces, Player, Count) :-
    findall(Player, member(piece(Player, none, none, none), Pieces), UnplacedPieces),
    length(UnplacedPieces, Count).

% count_placed_pieces(+Pieces, +Player, -Count)
% Counts the number of placed pieces for the specified player.
count_placed_pieces(Pieces, Player, Count) :-
    findall(Player, (member(piece(Player, Row, _, _), Pieces), Row \= none, Row \= off), PlacedPieces),
    length(PlacedPieces, Count).

% count_crossed_pieces(+Pieces, +Player, -Count)
% Counts the number of pieces that crossed the field for the specified player.
count_crossed_pieces(Pieces, Player, Count) :-
    findall(Player, member(piece(Player, off, off, off), Pieces), CrossedPieces),
    length(CrossedPieces, Count).
