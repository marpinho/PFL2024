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
    piece(player1, 1, 1, topLeft),
    piece(player2, 2, 2, bottomRight)
]).

% Generate a 4x4 board with randomized disks
generate_board(Board) :-
    findall(Disk, (between(1, 16, _), random_disk(Disk)), FlatBoard),
    partition(4, FlatBoard, Board).

% Randomly generate a disk with accessible and inaccessible slots
random_disk([Slot1, Slot2, Slot3, Slot4]) :-
    random_member(Slot1, [player1Exclusive, player2Exclusive, neutral, inaccessible]),
    random_member(Slot2, [player1Exclusive, player2Exclusive, neutral, inaccessible]),
    random_member(Slot3, [player1Exclusive, player2Exclusive, neutral, inaccessible]),
    random_member(Slot4, [player1Exclusive, player2Exclusive, neutral, inaccessible]).

% Partition a flat list into a 4x4 board
partition(_, [], []).
partition(Size, List, [Row|Rest]) :-
    length(Row, Size),
    append(Row, Tail, List),
    partition(Size, Tail, Rest).

% Initialize player pieces
initialize_pieces([Player1Pieces, Player2Pieces]) :-
    initialize_player_pieces(player1, Player1Pieces),
    initialize_player_pieces(player2, Player2Pieces).

initialize_player_pieces(Player, Pieces) :-
    length(Pieces, 5),
    maplist(=(piece(Player, none, none, none)), Pieces).