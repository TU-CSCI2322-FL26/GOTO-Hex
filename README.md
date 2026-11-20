FOR FILES REPRESENTING GAMES OF HEX:
- The filetype must be .csv
- The first line must consist of the active player and the board's size
- The second line must be a list of the positions the Horizontal player has placed a tile
- The third line must be as 6, but for the Vertical player
- For 4, separate the player (Hori or Vert) and the board's size by a comma with NO SPACES
- For 5 and 6, positions should be of form x1 y1,x2 y2,... where the X value comes first, then a
  space followed by the Y value, and only a comma separates each pair
    (the X value is the column of the tile, and the Y is the row)
- There must be at least three lines, regardless of whether any moves have been made by a given
  player
- Anything beyond the third line is ignored, and thus can be used for a description of the game



Reference for game files:
TestGame1.csv contains the empty game of size 11 on Hori's turn
TestGame2.csv contains a game Vert will win in 1 move
TestGame3.csv contains a game Vert will win in 2 moves
TestGame4.csv contains a game Hori will win in 3 moves
TestGame5.csv contains a game Hori will win in 5 moves
