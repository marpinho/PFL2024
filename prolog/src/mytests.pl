
:- [game].
:- [display].
:- [validation].
:- [samples].
:- [execution].
:- use_module(library(lists)).


mypieces([
    piece(player1, 2, 2, topLeft), % Placed on the board
    piece(player2, none, none, none) % Unplaced
]).

myboard([
    [disc(neutral, inaccessible, player1Exclusive, player2Exclusive), disc(neutral, neutral, neutral, inaccessible)],
    [disc(player1Exclusive, neutral, inaccessible, player2Exclusive), disc(neutral, player2Exclusive, neutral, inaccessible)]
]).

test :-
    sample_board2(Board),
    sample_pieces(Pieces),
    display_board(Board, Pieces, 1),
    move_piece(game_state(Board, Pieces, player1, move, 3), piece(player1, 2, 2, topLeft), right, NewGameState).
    display_board(NewBoard, NewPieces, 1).


