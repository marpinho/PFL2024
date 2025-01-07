
/*
* display_game(+GameState)
 The game display prints the board and a simple caption. The board is printed row by row, recursively.
*/
display_game(game_state(Board, Pieces, CurrentPlayer, CurrentPhase, RemainingMoves)) :-
    nl,
    format('Current Player: ~w~n', [CurrentPlayer]),  
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
    Player \= '',  % The empty string indicates that the piece isnt present
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





