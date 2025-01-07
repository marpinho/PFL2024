:- [display].
:- [validation].
:- [samples].
:- [execution].
:- use_module(library(lists)).


/*
 initial_state(+GameConfig, -GameState)
 display_game(+GameState)
 move(+GameState, +Move, -NewGameState).
 valid_moves(+GameState, -ListOfMoves).
 game_over(+GameState, -Winner)
 value(+GameState, +Player, -Value)
 choose_move(+GameState, +Level, -Move)
*/

/* MENU SYSTEM */

play :-
    write('Welcome to Blütentanz!'), nl,
	nl,write('Choose the game mode: '),nl,nl,
	write('1. Human vs. Human'),nl,
	write('2. Human vs. Computer'),nl,
	write('3. Computer vs. Computer'),nl,
	write('0. Quit'),  
    read(GameMode),
    GameMode < 4,
    start(GameMode).

start(0) :-
    abort.
start(1) :- 
    initial_state(game_config(player1, player2), InitialState).
start(2) :- 
    write('Select AI difficulty (1: Easy, 2: Medium): '), nl,
    read(Difficulty),
    initial_state(game_config(player, ai(Difficulty)), InitialState).
start(3) :- 
    write('Select AI difficulty for Player 1 (1: Easy, 2: Medium): '), nl,
    read(Difficulty1),
    write('Select AI difficulty for Player 2 (1: Easy, 2: Medium): '), nl,
    read(Difficulty2),
    initial_state(game_config(ai1(Difficulty1), ai2(Difficulty2)), InitialState).
start(_) :-
    nl, write('--INVALID INPUT--Please try again. '), nl,
    play.

/* BASIC SETUP */
    % game_config(+Player1, +Player2)
    % game_state(+Board, +Pieces, +CurrentPlayer, +CurrentStage , +MovesLeft )

initial_state(game_config(Player1, Player2), game_state(Board, Pieces, Player1, rotate, 3)) :-
    sample_board(Board),
    sample_pieces(Pieces),
    game_cycle(game_config(Player1, Player2), game_state(Board, Pieces, Player1, rotate, 3)).

/* GAME LOOP */

% Handles the turn-based game loop
% Updates the game state until the game is over
game_cycle(game_config(Player1, Player2), game_state(Board, Pieces, CurrentPlayer, Phase, MovesLeft)) :-
    display_game(game_state(Board, Pieces, CurrentPlayer, Phase, MovesLeft)),
    game_outcome(game_state(Board, Pieces, CurrentPlayer, Phase, MovesLeft), Outcome),
    handle_outcome(Outcome, game_config(Player1, Player2), game_state(Board, Pieces, CurrentPlayer, Phase, MovesLeft)).

game_cycle(game_state(Board, Pieces, CurrentPlayer, Phase, MovesLeft)) :-
    display_game(game_state(Board, Pieces, CurrentPlayer, Phase, MovesLeft)),
    game_outcome(game_state(Board, Pieces, CurrentPlayer, Phase, MovesLeft), Outcome),
    handle_outcome(Outcome, game_config(player1, player2), game_state(Board, Pieces, CurrentPlayer, Phase, MovesLeft)).

/* END CONDITIONS*/

% Determine game outcome
game_outcome(game_state(_, Pieces, _, _, _), game_over(Winner)) :-
    game_over(game_state(_, Pieces, _, _, _), Winner),
    Winner \= none.
game_outcome(game_state(_, _, CurrentPlayer, _, _), continue(CurrentPlayer)).

% Check if the game is over
% game_over(+GameState, -Winner)
game_over(game_state(_, Pieces, CurrentPlayer, _, _), CurrentPlayer) :-
    pieces_on_opponent_side(Pieces, CurrentPlayer).
game_over(_, none).


% Check if all pieces are on the opponents side
pieces_on_opponent_side(Pieces, Player) :-
    findall(_, (member(piece(Player, off, off, _), Pieces)), OffBoardPieces),
    length(OffBoardPieces, Count),
    Count >= 4.

handle_outcome(game_over(Winner), _, _) :-
    format('Game Over! Winner: ~w~n', [Winner]).

/* PLAY*/

handle_outcome(continue(CurrentPlayer), _, game_state(Board, Pieces, CurrentPlayer, rotate, MovesLeft)) :-
    prompt_rotation(CurrentPlayer, Rotation),
    validate_and_apply_move(game_state(Board, Pieces, CurrentPlayer, rotate, MovesLeft), move(rotation, Rotation)).

handle_outcome(continue(CurrentPlayer), game_config(Player1, Player2), game_state(Board, Pieces, CurrentPlayer, move, MovesLeft)) :-
    prompt_movement(CurrentPlayer, MovesLeft, Pieces, Board, UpdatedPieces),
    game_outcome(game_state(Board, UpdatedPieces, CurrentPlayer, _, MovesLeft), Outcome),
    handle_second_outcome(Outcome, game_config(Player1, Player2), game_state(Board, UpdatedPieces, CurrentPlayer, _, _)).

handle_second_outcome(continue(CurrentPlayer), game_config(Player1, Player2), game_state(Board, Pieces, CurrentPlayer, _, _)) :-
    switch_player(CurrentPlayer, game_config(Player1, Player2), NextPlayer),
    game_cycle(game_state(Board, Pieces, NextPlayer, rotate, 3)).

    
handle_second_outcome(game_over(Winner),_ ,_) :-
    format('Game Over! Winner: ~w~n', [Winner]).



% Validate and apply move
validate_and_apply_move(game_state(Board, Pieces, CurrentPlayer, Phase, MovesLeft), Move) :-
    valid_moves(game_state(Board, Pieces, CurrentPlayer, Phase, MovesLeft), Moves),
    member(Move, Moves),
    move(game_state(Board, Pieces, CurrentPlayer, Phase, MovesLeft), Move, NewGameState),
    game_cycle(NewGameState).
validate_and_apply_move(GameState, _) :-
    write('Invalid move. Try again.'), nl,
    game_cycle(GameState).

% Switch player based on game state
switch_player(CurrentPlayer,game_config(Player1, Player2), NextPlayer) :-
    (CurrentPlayer = Player1 -> NextPlayer = Player2 ; NextPlayer = Player1).

/* ROTATION */

% Prompt for rotation
prompt_rotation(Player, Rotation) :-
    write(Player), write(', do you want to rotate a row or a column?'), nl,
    write('1. Row'), nl,
    write('2. Column'), nl,
    read(Type),
    handle_rotation_type(Type, Player, Rotation).

% Handle rotation type selection
handle_rotation_type(1, Player, row(Index)) :-
    write(Player), write(', enter the row number (1-4) to rotate: '), nl,
    read(Index),
    validate_rotation(row(Index)).
handle_rotation_type(2, Player, col(Index)) :-
    write(Player), write(', enter the column number (1-4) to rotate: '), nl,
    read(Index),
    validate_rotation(col(Index)).
handle_rotation_type(_, Player, Rotation) :-
    write('Invalid choice. Try again.'), nl,
    prompt_rotation(Player, Rotation).

% Validate rotation input
validate_rotation(row(Index)) :-
    Index >= 1, Index =< 4.
validate_rotation(col(Index)) :-
    Index >= 1, Index =< 4.
validate_rotation(_) :-
    write('Invalid row or column. Must be between 1 and 4.'), nl, fail.

/* MOVEMENT ACTIONS */

% Prompt for movement actions
prompt_movement(_, 0, Pieces, _, Pieces) :-
    write('No movement points left. Ending turn.'), nl.
prompt_movement(Player, MovesLeft, Pieces, Board, UpdatedPieces) :-
    write('Do you want to place a new piece or move an existing piece?'), nl,
    write('1. Place a new piece'), nl,
    write('2. Move an existing piece'), nl,
    read(Action),
    handle_action_choice(Action, Player, MovesLeft, Pieces, Board, UpdatedPieces, RemainingMoves),
    prompt_movement(Player, RemainingMoves, UpdatedPieces, Board, UpdatedPieces).

% Handle action choice
handle_action_choice(1, Player, MovesLeft, Pieces, Board, UpdatedPieces, RemainingMoves) :-
    prompt_place_piece(Player, Pieces, Board, UpdatedPieces),
    RemainingMoves is MovesLeft - 1.
handle_action_choice(2, Player, MovesLeft, Pieces, Board, UpdatedPieces, RemainingMoves) :-
    prompt_piece_selection(Player, Pieces, SelectedPiece),
    prompt_movement_action(Player, SelectedPiece, Pieces, Board, MovesLeft, UpdatedPieces, RemainingMoves).
handle_action_choice(_, Player, MovesLeft, Pieces, Board, UpdatedPieces, _) :-
    write('Invalid choice. Try again.'), nl,
    prompt_movement(Player, MovesLeft, Pieces, Board, UpdatedPieces).

/* PLACE A NEW PIECE */

% Prompt for placing a new piece
prompt_place_piece(Player, Pieces, Board, UpdatedPieces) :-
    write('Enter the row to place your piece: '), nl,
    read(Row),
    write('Enter the column to place your piece: '), nl,
    read(Col),
    write('Enter the position on the disc (e.g., topLeft, bottomRight): '), nl,
    read(Position),
    validate_starting_position(Player, Board, Row, Col, Position, Pieces, IsValid),
    handle_place_piece_validation(IsValid, Player, Pieces, Row, Col, Position, Board, UpdatedPieces).

% Handle place piece validation
handle_place_piece_validation(true, Player, Pieces, Row, Col, Position, _, UpdatedPieces) :-
    place_piece(Player, Row, Col, Position, Pieces, UpdatedPieces).
handle_place_piece_validation(false, Player, Pieces, _, _, _, Board, UpdatedPieces) :-
    write('Invalid position. Try again.'), nl,
    prompt_place_piece(Player, Pieces, Board, UpdatedPieces).


/* MOVE A PLACED PIECE */

% Prompt for selecting a piece
prompt_piece_selection(Player, Pieces, SelectedPiece) :-
    write('Enter the row of the piece you want to move: '), nl,
    read(Row),
    write('Enter the column of the piece you want to move: '), nl,
    read(Col),
    write('Enter the position on the disc (e.g., topLeft, bottomRight): '), nl,
    read(Position),
    validate_piece_selection(Player, Pieces, Row, Col, Position, SelectedPiece).

% Validate piece selection
validate_piece_selection(Player, Pieces, Row, Col, Position, SelectedPiece) :-
    member(piece(Player, Row, Col, Position), Pieces),
    SelectedPiece = piece(Player, Row, Col, Position).
validate_piece_selection(Player, Pieces, _, _, _, SelectedPiece) :-
    write('No valid piece found at that location. Try again.'), nl,
    prompt_piece_selection(Player, Pieces, SelectedPiece).

% Prompt for movement action
prompt_movement_action(Player, piece(Player, Row, Col, Position), Pieces, Board, MovesLeft, UpdatedPieces, NewMovesLeft) :-
    write('Select a direction to move the piece:'), nl,
    findall(Direction, valid_square(Board, Player, Row, Col, Direction, Pieces), Directions),
    display_directions(Directions),
    read(Direction),
    validate_movement_action(Player, Board, Row, Col, Direction, IsValid),
    handle_direction_validation(IsValid, Player, piece(Player, Row, Col, Position), Pieces, Board, MovesLeft, Direction, UpdatedPieces, NewMovesLeft).

% Display available directions
display_directions([]) :-
    write('No valid directions available.'), nl.
display_directions([Dir|Dirs]) :-
    write('- '), write(Dir), nl,
    display_directions(Dirs).

% Handle validation for movement direction
handle_direction_validation(true, Player, piece(Player, Row, Col, Position), Pieces, Board, MovesLeft, Direction, game_state(Board, UpdatedPieces, Player, move, NewMovesLeft)) :-
    move_piece(game_state(Board, Pieces, Player, move, MovesLeft), piece(Player, Row, Col, Position), [Direction], game_state(Board, UpdatedPieces, Player, move, NewMovesLeft)).

handle_direction_validation(false, Player, piece(Player, Row, Col, Position), Pieces, Board, MovesLeft, _, game_state(Board, Pieces, Player, move, MovesLeft)) :-
    write('Invalid move. Try again.'), nl,
    prompt_movement_action(Player, piece(Player, Row, Col, Position), Pieces, Board, MovesLeft, game_state(Board, Pieces, Player, move, MovesLeft)).


