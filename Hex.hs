module Hex where
import Debug.Trace
import Data.Maybe

data Player = Hori | Vert deriving (Show, Eq)
type Board = [Cell]
type Cell = (Position, Player)
type Game = (Board, Player, Integer)
type Position = (Integer, Integer)
type Move = Position
type Winner = Maybe Player


-- testing code --
testGame = (testBoard, Hori, 11)
testBoard = [ ((x,y), if mod x 2 == 0 then Hori else Vert) | x<-[0..10], y<-[0..10] ]
testMove = (4, 4)
-- testing code --

-- makeMove takes a Game and a Move, and returns the Game after the move concluded 
-- returns an unchanged Game state if the attempted move is invalid
makeMove :: Game -> Move -> Maybe Game
makeMove (board, player, size) move
    | validMove (board, player, size) move = 
        Just ((move, player):board, switchPlayer player, size)
    | otherwise = Nothing
  where switchPlayer :: Player -> Player
        switchPlayer Hori = Vert
        switchPlayer Vert = Hori

-- validMove checks if a move is able to be played, by checking if the cell is already occupied,
-- and if the position is inside of the board size 
validMove :: Game -> Move -> Bool
validMove (board, _, size) move =
    fst move >= 0 && fst move < size &&
    snd move >= 0 && snd move < size &&
    hasCell board move

-- legalMoves takes a Game state and returns the list of all valid moves
legalMoves :: Game -> [Move]
legalMoves game@(_, _, size) =
    [ (x, y) | x <- [0..size-1], y <- [0..size-1], validMove game (x, y) ]


-- this function takes in a Game
-- then returns either Just the winner of that game or Nothing
getWinner :: Game -> Winner
getWinner (game@(board, player, boardsize)) = lookup True (zip wins $ map snd starts)
  where starts = [ cell | (cell@((x,y),p))<-board, case p of Hori -> x == 0
                                                             Vert -> y == 0 ]
        wins = map (\x -> auxWins game [] (x:[])) starts
        
        -- this function takes in a list of cells (or a single cell :[] to start) to check and a game
        -- then returns whether or not the cells connect the respective side(s)
        auxWins :: Game -> [Cell] -> [Cell] -> Bool
        auxWins _ acc [] = False
        auxWins (game@(board, _, bsize)) acc ((curr@((x,y),p)):toCheck) =
          let tmp = [ x | x<-(adjSimilar board curr), not (elem x acc) ]
              newChecks = tmp ++ toCheck
              newAcc = tmp ++ acc
              winDimension = case p of Hori -> x
                                       Vert -> y
          in if (winDimension == bsize - 1)
               then True
               else auxWins game newAcc newChecks

        -- this function takes in a cell and a boardstate
        -- then returns a list of all cells with the same player adjacent to the given cell
        adjSimilar :: Board -> Cell -> [Cell]
        adjSimilar board (cell@(_,player)) = [ x | (x@(_,pl))<-(adjacents board cell), pl == player ]

        -- this function takes in a position and a boardstate
        -- then returns a list of all cells adjacent to the given position
        adjacents :: Board -> Cell -> [Cell]
        adjacents board ((x,y),_) = catMaybes $ map (getCell board) ((x-1,y):(x+1,y):(x,y-1):(x+1,y-1):(x-1,y+1):(x,y+1):[])

-- this function takes in a board and position
-- then returns whether or not a cell exists in that position
hasCell :: Board -> Position -> Bool
hasCell board pos = case getCell board pos of Nothing -> False
                                              _ -> True

-- this function takes in a board and position
-- then returns Just the cell at that position (or Nothing)
getCell :: Board -> Position -> Maybe (Position, Player)
getCell [] _ = Nothing
getCell ((cell@((a,b),_)):rest) (pos@(x,y)) = if a == x && b == y then Just cell else getCell rest pos
