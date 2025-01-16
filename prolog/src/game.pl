
/*
 game_state(+Board, +Pieces, +Player, +Opponent, +NumberOfMoves)
 move(rotate_row, +Index)
 move(rotate_col, +Index)
 move(place_piece, +RowIndex, +ColIndex, +PosInDisc)
 move(move_piece, +RowIndex, +ColIndex, +PosInDisc, +Direction)

*/
:- [execution].
:- [display].

% play
play :-
    nl, write('Welcome to Blutentanz!'), nl,nl,
    prompt_game_mode(GameConfig),
    initial_state(GameConfig, GameState), !,
    start(GameState).
play:-
    write('Invalid game mode!'), nl,nl,
    play.

% initial_state(+GameConfig, -GameState)
initial_state(0, _) :-
    !, write('Goodbye!'), abort.
initial_state(1, GameState) :-
    generate_board(Board),
    initialize_pieces(player1, player2, Pieces),
    GameState = game_state(Board, Pieces, player1, player2, 4).
initial_state(2, GameState) :-
    prompt_ai_level(Level),
    generate_board(Board),
    initialize_pieces(player1, computer2-Level, Pieces),
    GameState = game_state(Board, Pieces, player1, computer2-Level, 4).
initial_state(3, GameState) :-
    write('For Player 1'), nl,
    prompt_ai_level(Ai1Level),
    write('For Player 2'), nl,
    prompt_ai_level(Ai2Level),
    generate_board(Board),
    initialize_pieces(computer1-Ai1Level, computer2-Ai2Level, Pieces),
    GameState = game_state(Board, Pieces, computer1-Ai1Level, computer2-Ai2Level, 4).
initial_state(_,_):-
    !, write('Invalid game mode!'), nl,
    play.

start(GameState) :-
    display_game(GameState),                        
    player_turn(GameState, NewGameState),
    game_over(NewGameState, Output),
    handle_game_over(Output),
    start(NewGameState).

% game_over(+GameState, -Winner)
% Case 1: Current player wins by crossing 4 pieces
game_over(game_state(_, Pieces, CurrentPlayer, _, _), CurrentPlayer) :-
    count_crossed_pieces(Pieces, CurrentPlayer, CPcrossed),
    CPcrossed >= 4.
% Case 2: Opponent wins because the current player has no valid moves
game_over(game_state(Board, Pieces, CurrentPlayer, Opponent, NumberOfMoves), Opponent) :-
    valid_moves(game_state(Board, Pieces, CurrentPlayer, Opponent, NumberOfMoves), []), % No valid moves left
    write('No valid moves left!'), nl.
% Case 3: No winner yet
game_over(_, none).

% handle_game_over(+Outcome)
handle_game_over(none). % keep playing
handle_game_over(Winner) :-
    Winner \= none,  % Ensure Winner is not 'none'.
    write('Congratulations! '), 
    write(Winner), 
    write(' wins!'), fail.

% AI turn
player_turn(game_state(Board, Pieces, N-L, Opponent, NumberOfMoves), NewGameState) :-
    (N = computer1 ; N = computer2), !,
    choose_move(game_state(Board, Pieces, N-L, Opponent, NumberOfMoves), L, Move),
    write('AI '), write(N), write(' chose: '), write(Move), nl,
    move(game_state(Board, Pieces, N-L, Opponent, NumberOfMoves), Move, NewGameState).

% Player riotation turn
player_turn(game_state(Board, Pieces, Player, Opponent, 4), NewGameState) :-
    !, prompt_rotation(Type, Index),
    move(game_state(Board, Pieces, Player, Opponent, 4), move(Type, Index), NewGameState).

% Player placement/movement turn
player_turn(game_state(Board, Pieces, Player, Opponent, NumberOfMoves), NewGameState) :-
    between(1, 3, NumberOfMoves),
    prompt_movement_type(MovementType),
    player_turn(game_state(Board, Pieces, Player, Opponent, NumberOfMoves), MovementType, NewGameState).

% in case of invalid play, try again    
player_turn(GameState, NewGameState):-
    write('Try again!'), nl,
    player_turn(GameState, NewGameState).

% Player Place piece
player_turn(game_state(Board, Pieces, Player, Opponent, NumberOfMoves), place_piece, NewGameState) :-
    validate_has_unplaced_pieces(Pieces, Player),!,
    prompt_basic_movement_config(RowIndex, ColIndex, PosInDisc),
    validate_starting_position(Board, Pieces, Player, RowIndex, ColIndex, PosInDisc),
    move(game_state(Board, Pieces, Player, Opponent, NumberOfMoves), move(place_piece, RowIndex, ColIndex, PosInDisc), NewGameState).

% Player Move piece
player_turn(game_state(Board, Pieces, Player, Opponent, NumberOfMoves), move_piece, NewGameState) :-
    validate_has_placed_pieces(Pieces, Player),!,
    prompt_basic_movement_config(RowIndex, ColIndex, PosInDisc),
    validate_piece_exists(Pieces, Player, RowIndex, ColIndex, PosInDisc),!,
    prompt_movement_direction(Direction),
    move(game_state(Board, Pieces, Player, Opponent, NumberOfMoves), move(move_piece, RowIndex, ColIndex, PosInDisc, Direction), NewGameState).
player_turn(GameState, _, NewGameState):-
    write('Try again!'), nl,
    player_turn(GameState, NewGameState).

% move( +GameState, +Move, -NewGameState)
% Determines the new game state after a move has been made.

% Rotation phase
move(game_state(Board, Pieces, Player, Opponent, 4), move(rotate_row, Index) , NewGameState) :-
    rotate_row(Board, Pieces, Index, NewBoard, UpdatedPieces),
    NewGameState = game_state(NewBoard, UpdatedPieces, Player, Opponent, 3).
move(game_state(Board, Pieces, Player, Opponent, 4), move(rotate_col, Index) , NewGameState) :-
    rotate_column(Board, Pieces, Index, NewBoard, UpdatedPieces),
    NewGameState = game_state(NewBoard, UpdatedPieces, Player, Opponent, 3).

%  Placement/Movement phase and change player turn   
move(game_state(Board, Pieces, Player, Opponent, 1), move(place_piece, RowIndex, ColIndex, PosInDisc), NewGameState) :-
    place_piece(Board, Pieces, Player, RowIndex, ColIndex, PosInDisc, UpdatedPieces),
    NewGameState = game_state(Board, UpdatedPieces, Opponent, Player, 4).
move(game_state(Board, Pieces, Player, Opponent, 1), move(move_piece, RowIndex, ColIndex, PosInDisc, Direction), NewGameState) :-
    move_piece(Board, Pieces, Player, RowIndex, ColIndex, PosInDisc, Direction, UpdatedPieces),
    NewGameState = game_state(Board, UpdatedPieces, Opponent, Player, 4).

% Placement/Movement phase
move(game_state(Board, Pieces, Player, Opponent, NumberOfMoves), move(place_piece, RowIndex, ColIndex, PosInDisc), NewGameState) :-
    between(2, 3, NumberOfMoves),
    place_piece(Board, Pieces, Player, RowIndex, ColIndex, PosInDisc, UpdatedPieces),
    NumberOfMovesLeft is NumberOfMoves - 1,
    NewGameState = game_state(Board, UpdatedPieces, Player, Opponent, NumberOfMovesLeft).
move(game_state(Board, Pieces, Player, Opponent, NumberOfMoves), move(move_piece, RowIndex, ColIndex, PosInDisc, Direction), NewGameState) :-
    between(2, 3, NumberOfMoves),
    move_piece(Board, Pieces, Player, RowIndex, ColIndex, PosInDisc, Direction, UpdatedPieces),
    NumberOfMovesLeft is NumberOfMoves - 1,
    NewGameState = game_state(Board, UpdatedPieces, Player, Opponent, NumberOfMovesLeft).

% choose_move(+GameState, +Level, -Move)
% Chooses a move for the given GameState and Level.

% Level 1: Random valid move.
choose_move(GameState, 1, Move) :-
    valid_moves(GameState, Moves),
    random_member(Move, Moves). % Level 1: Random selection of valid moves.

% Level 2: Greedy move based on value/3.
choose_move(GameState, 2, Move) :-
    valid_moves(GameState, Moves),
    GameState = game_state(_, _, Player, _, _), % Extract the current player.
    findall(Value-M, 
        (member(M, Moves), simulate_move(GameState, M, NewGameState), value(NewGameState, Player, Value)), 
        ScoredMoves),
    sort(ScoredMoves, SortedMoves), % Sort moves by their values.
    reverse(SortedMoves, [_BestScore-Move|_]). % Select the move with the highest value.

