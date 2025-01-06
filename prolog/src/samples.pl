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
    [disc(player1Exclusive, neutral, inaccessible, player2Exclusive), disc(neutral, inaccessible, player1Exclusive, player2Exclusive)],
    [disc(player2Exclusive, inaccessible, neutral, player1Exclusive), disc(inaccessible, neutral, player2Exclusive, player1Exclusive)]
]).

sample_pieces([
    piece(player1, 1, 1, topLeft),
    piece(player2, 2, 2, bottomRight)
]).
