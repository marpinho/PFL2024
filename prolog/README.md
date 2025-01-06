## Game Details

- in *Blütentanz*, you want to lead your figures to reach your opponent's side of the field.

- To set up the game, sixteen discs are placed onto the game board at random; Each disc has 4 slots:
    - blue, only accessible to the Blue Player
    - orange, only accessible to the Orange Player
    - gray, accessible to both Players
    - null, innacessible to both Players

- Your goal is to move four of your five figures off the edge of the game board near your opponent. Whoever does this first wins.

On a turn each player must:
- First choose a row or column and rotate all its tiles 90º, with any player figures on those tiles rotating as well.
- Next, you have up to three movement points that you can use on 1-3 of your figures. You can:
    - place a figure on the accessible places of your edge of the board. 
    - keep moving a figure orthogonally to accessible places.

More details are available in [Board Game Geek](https://boardgamegeek.com/boardgame/428363/blutentanz). No extra installations are required.




## Game State Representation
A Board is a list of 16 independent Disks organized in a 4x4 square. These disks have four positions (which we refer to as squares in the code, even though a disk is circular): Top Left, Top Right, Bottom Left, Bottom Right. The disks can rotate 90º clockwise, shuffling the order of its squares.

Each square in a Disk features a blossom, which controls which players can move pieces there. There is one unavalable square, one reserved for each player and another one available for both.

Each player has five pieces. The game stores the following information regarding pieces:
- Player
- Disk (Row and Column)
- Square (Top Left, Top Right, Bottom Left, Bottom Right)

More pieces could be added without breaking the game. Unnused pieces have their position marked as 'none' and pieces that reach the opponent's side of the board are ...

The game state also includes turn information:
- the currently active Player
- the phase of his turn (rotate or move piece)
- the remaining moves (only relevant when in 'move piece' phase)

## Move Representation
Moves are stored as move([type], [details]),  There are two types of moves:
- rotations which can be applied to rows and columns, on a given index. An example of a rotation is move(rotation, row(2)), which applies to the second row in the board.
- piece movements, issue!

## User Interaction
WIP

| Name                | UP ID     | Contribution    |
| ------------------- | --------- | --------------- |
| Guilherme Magalhães | 202005285 | 50%             |
| Margarida Pinho     | 201704599 | 50%             |
