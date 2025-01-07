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

% Sample board and pieces for testing

sample_board([
    [disc(neutral, player1Exclusive, player2Exclusive, inaccessible), disc(player1Exclusive, neutral, inaccessible, player2Exclusive), disc(player2Exclusive, inaccessible, neutral, player1Exclusive), disc(inaccessible, player2Exclusive, player1Exclusive, neutral)],
    [disc(player1Exclusive, neutral, inaccessible, player2Exclusive), disc(inaccessible, player2Exclusive, neutral, player1Exclusive), disc(player2Exclusive, player1Exclusive, inaccessible, neutral), disc(neutral, inaccessible, player2Exclusive, player1Exclusive)],
    [disc(player2Exclusive, inaccessible, neutral, player1Exclusive), disc(neutral, player1Exclusive, player2Exclusive, inaccessible), disc(inaccessible, neutral, player1Exclusive, player2Exclusive), disc(player1Exclusive, player2Exclusive, neutral, inaccessible)],
    [disc(inaccessible, player2Exclusive, player1Exclusive, neutral), disc(player2Exclusive, inaccessible, neutral, player1Exclusive), disc(neutral, player1Exclusive, player2Exclusive, inaccessible), disc(player1Exclusive, neutral, inaccessible, player2Exclusive)]
]).

sample_pieces([
    piece(player1, 1, 2, topLeft),
    piece(player2, 1, 3, bottomRight),
    piece(player1, 2, 1, topRight),
    piece(player2, 2, 4, bottomLeft),
    piece(player1, none, none, none) % Unplaced piece
]).

sample_board2([
    [disc('N', 'X', 'E-1', 'E-2'), disc('E-1', 'N', 'X', 'E-2'), disc('E-2', 'X', 'N', 'E-1'), disc('X', 'E-2', 'E-1', 'N')],
    [disc('N', 'E-1', 'E-2', 'X'), disc('X', 'E-2', 'N', 'E-1'), disc('E-2', 'X', 'N', 'E-1'), disc('N', 'X', 'E-2', 'E-1')],
    [disc('E-1', 'X', 'N', 'E-2'), disc('N', 'E-2', 'E-1', 'X'), disc('X', 'N', 'E-2', 'E-1'), disc('E-1', 'E-2', 'X', 'N')],
    [disc('X', 'N', 'E-1', 'E-2'), disc('E-2', 'E-1', 'X', 'N'), disc('N', 'X', 'E-1', 'E-2'), disc('E-1', 'E-2', 'N', 'X')]
]).
