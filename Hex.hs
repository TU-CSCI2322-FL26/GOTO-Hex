import Debug.Trace
import Data.Maybe

data Player = Hori | Vert deriving (Show, Eq)
type Board = [Cell]
type Cell = (Position, Player)
type Game = (Board, Player, Integer)
type Position = (Integer, Integer)
type Move = Position
type Winner = Maybe Player

testGame = ([], Hori, 11)
testGame2 = ([((1,1),Vert)], Hori, 11)
testMove = (4, 4)

-- makeMove takes a Game and a Move, and returns the Game after the move concluded 
-- returns an unchanged Game state if the attempted move is invalid
makeMove :: Game -> Move -> Game
makeMove (board, player, size) move
    | validMove (board, player, size) move = 
        (board ++ [(move, player)], switchPlayer player, size)
    | otherwise = (board, player, size)

-- switchPlayer swaps the current player
switchPlayer :: Player -> Player
switchPlayer Hori = Vert
switchPlayer Vert = Hori

-- validMove checks if a move is able to be played, by checking if the cell is already occupied,
-- and if the position is inside of the board size 
validMove :: Game -> Move -> Bool
validMove (board, _, size) move =
    move `notElem` map fst board &&
    fst move >= 0 && fst move < size &&
    snd move >= 0 && snd move < size


-- legalMoves takes a Game state and returns the list of all valid moves
legalMoves :: Game -> [Move]
legalMoves game@(_, _, size) =
    [ (x, y) | x <- [0..size-1], y <- [0..size-1], validMove game (x, y) ]