% Test file for Blütentanz functionalities

% Test initial_state/2

test_initial_state_valid :-
    initial_state(game_config(player1, player2), GameState),
    GameState = game_state(Board, Pieces, player1, rotate, 3),
    length(Board, 4),
    length(Pieces, 5),
    write('initial_state/2: Passed'), nl.

% Test valid_moves/2

test_valid_moves_rotation_phase :-
    initial_state(game_config(player1, player2), GameState),
    valid_moves(GameState, Moves),
    length(Moves, 8),  % 4 rows + 4 columns rotations
    member(move(rotation, row(1)), Moves),
    member(move(rotation, col(4)), Moves),
    write('valid_moves/2 (rotation phase): Passed'), nl.

test_valid_moves_movement_phase :-
    initial_state(game_config(player1, player2), game_state(Board, Pieces, player1, move, 3)),
    valid_moves(game_state(Board, Pieces, player1, move, 3), Moves),
    length(Moves, 5),  % Based on sample pieces
    member(move(movement, (1, 2), topLeft), Moves),
    write('valid_moves/2 (movement phase): Passed'), nl.

% Test move/3

test_move_rotation_row :-
    initial_state(game_config(player1, player2), GameState),
    move(GameState, move(rotation, row(1)), NewGameState),
    NewGameState = game_state(NewBoard, _, _, _, _),
    nth1(1, NewBoard, RotatedRow),
    RotatedRow \= Board,
    write('move/3 (rotation row): Passed'), nl.

test_move_piece_valid :-
    initial_state(game_config(player1, player2), game_state(Board, Pieces, player1, move, 3)),
    move(game_state(Board, Pieces, player1, move, 3), move(movement, (1, 2), right), NewGameState),
    NewGameState = game_state(_, NewPieces, _, _, _),
    member(piece(player1, 1, 3, topRight), NewPieces),
    write('move/3 (piece movement): Passed'), nl.

% Test game_over/2

test_game_not_over_initial :-
    initial_state(game_config(player1, player2), GameState),
    (\+ game_over(GameState, _) -> write('game_over/2 (not over): Passed'), nl ; write('game_over/2 (not over): Failed'), nl).

test_game_over_player1_wins :-
    sample_game_state(game_state(Board, Pieces, player1, _, _)),
    append([piece(player1, off, off, none), piece(player1, off, off, none), piece(player1, off, off, none), piece(player1, off, off, none)], Pieces, WinningPieces),
    (game_over(game_state(Board, WinningPieces, player1, _, _), Winner), Winner == player1 -> write('game_over/2 (player1 wins): Passed'), nl ; write('game_over/2 (player1 wins): Failed'), nl).

% Test display_game/1

test_display_game :-
    initial_state(game_config(player1, player2), GameState),
    display_game(GameState),
    write('display_game/1: Visual Check Required'), nl.

% Run all tests
run_tests :-
    test_initial_state_valid,
    test_valid_moves_rotation_phase,
    test_valid_moves_movement_phase,
    test_move_rotation_row,
    test_move_piece_valid,
    test_game_not_over_initial,
    test_game_over_player1_wins,
    test_display_game.
