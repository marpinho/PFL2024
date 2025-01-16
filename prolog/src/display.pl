:- [validation].

% display_game(+GameState)
% Displays the current state of the game.
display_game(game_state(Board, Pieces, CurrentPlayer, Opponent, NumberOfMoves)) :-
    nl,
    display_pieces(Pieces, CurrentPlayer, Opponent),nl,nl,
    write(' Player 1 field'), nl, nl,
    display_board(Board, Pieces), nl,
    write(' Player 2 field'), nl, nl,
    display_legend, nl,
    display_phase(NumberOfMoves, CurrentPlayer).


/*PHASES*/

display_phase(NumberOfMoves, Player):-
    infer_phase(NumberOfMoves, Phase),
    piece_representation(Player, Piece),
    format('Current Player: ~w ~w~n', [Player, Piece]),
    format('Current Phase: ~w~n', [Phase]), 
    format('Remaining Moves: ~d~n', [NumberOfMoves]), nl.

% infer_phase(+NumberOfMoves, -Phase)
% Infers the current phase of the game based on the number of moves.
infer_phase(4, 'Rotation').
infer_phase(NumberOfMoves, 'Placement/Movement') :- NumberOfMoves < 4.


/*PIECES*/

% display_pieces(+Pieces, +CurrentPlayer, +Opponent)
% Displays the number of unplaced pieces for each player.
display_pieces(Pieces, CurrentPlayer, Opponent) :-
    display_player_pieces(Pieces, CurrentPlayer),
    display_player_pieces(Pieces, Opponent).

% display_player_pieces(+Pieces, +Player)
% Displays the number of unplaced and crossed pieces for the specified player.
display_player_pieces(Pieces, Player) :-
    piece_representation(Player, Piece),
    count_crossed_pieces(Pieces, Player, Crossed),
    count_unplaced_pieces(Pieces, Player, Unplaced),
    format(' ~w ~w  ', [Player, Piece]), nl,
    format(' No. pieces that crossed the field: ~d', [Crossed]),nl,
    format(' No. unplaced pieces: ~d', [Unplaced]),nl.


/*BOARD*/

% display_board(+Board, +Pieces)
% Displays the current state of the board and the pieces.
display_board(Board, Pieces):-
    write('       1         2         3         4'), nl,
    write('  +---------+---------+---------+---------+'), nl,
    print_board(Board, Pieces).

% print_board(+Board, +Pieces)
% Prints the board.
print_board(Board, Pieces):-
    print_board_row(Board, Pieces, 1).

% print_board_row(+Board, +Pieces, +RowIndex)
% Print the board row by row.
print_board_row([], _, _) :- !.
print_board_row([Row|Rest], Pieces, RowIndex) :- 
    write('  '),  
    display_disc_row(Row, Pieces, RowIndex, 1, top),
    format(' ~w|    +    |    +    |    +    |    +    |', [RowIndex]), nl,
    write('  '),
    display_disc_row(Row, Pieces, RowIndex, 1, bottom), % Each row occupies two lines in the output, that must be printed separately
    write('  +---------+---------+---------+---------+'), nl,
    NewRowIndex is RowIndex + 1,
    print_board_row(Rest, Pieces, NewRowIndex).

% display_disc_row(+Row, +Pieces, +RowIndex, +ColIndex, +Half)
display_disc_row([], _, _, _, _) :- % Base Case: no more squares, move to next line/row
    write('|'), nl.
display_disc_row([Disc|Rest], Pieces, RowIndex, ColIndex, Half) :- % Recursive Step: print the top or bottom half of a disk
    write('| '),
    print_disc_section(Disc, Pieces, RowIndex, ColIndex, Half), % Print the current disc section (top/bottom).
    NewColIndex is ColIndex + 1,
    display_disc_row(Rest, Pieces, RowIndex, NewColIndex, Half). % Recurse to the next disc.

% print_disc_section(+Disc, +Pieces, +Row, +Col, +Half)
print_disc_section(disc(TL, TR, _, _), Pieces, Row, Col, top) :- % Top-Half of the disk
    format_disc_square(TL, Row, Col, topLeft, Pieces, Left), 
    format_disc_square(TR, Row, Col, topRight, Pieces, Right),
    format('~w ~w ', [Left, Right]).
print_disc_section(disc(_, _, BL, BR), Pieces, Row, Col, bottom) :- % Bottom-Half of the disk
    format_disc_square(BL, Row, Col, bottomLeft, Pieces, Left),
    format_disc_square(BR, Row, Col, bottomRight, Pieces, Right),
    format('~w ~w ', [Left, Right]).  % Print the formatted values.

format_disc_square(_, Row, Col, Square, Pieces, Output) :-
    member(piece(Player, Row, Col, Square), Pieces),!,
    piece_representation(Player, Output). % Match found.
format_disc_square(Type, _, _, _, _, Output) :-
    square_representation(Type, Output).

% square_representation(+BlossomType, -Representation)
% Maps square types to their visual representation.
square_representation(n, ' N ').
square_representation(x, '   ').
square_representation(e1, ' O ').
square_representation(e2, ' B ').

% piece_representation(+PlayerPiece, -Representation)
% Maps player pieces to their visual representation.
piece_representation(player1, '-o-').
piece_representation(computer1-_, '-o-').
piece_representation(player2, '-b-').
piece_representation(computer2-_, '-b-').
piece_representation('', '   ').  % Empty square


% display_legend
% Displays the legend for square and piece types.
display_legend :-
    write(' N - Neutral '), nl,
    write(' X - Inaccessible '), nl,
    write(' O - Player 1 exclusive '), nl,
    write(' B - Player 2 exclusive '),nl.


  