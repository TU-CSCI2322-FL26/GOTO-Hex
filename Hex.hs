import Debug.Trace
import Data.Maybe
import Data.List

data Player = Hori | Vert deriving (Show, Eq)
type Board = [Cell]
type Cell = (Position, Player)
type Position = (Integer, Integer)
type Move = Position
type Winner = Maybe Player
type Game = (Board, Player, Integer)

testGame = (testBoard, Hori, 11)
testBoard = [ ((x,y), if mod x 2 == 0 then Hori else Vert) | x<-[0..10], y<-[0..10] ]

instance {-# OVERLAPS #-} Show Game where 
  show (board, player, boardSize) = 
    let strBoard = foldr (\line acc -> line ++ ("\n" ++ acc)) "" endLines
        endLines = map (\(l,r) -> putSortedRow l r 0) (zip absLines absRows)
        absRows = map sortRow (getRows 0)
        absLines = getLines 0
        putSortedRow line [] n = if (n >= boardSize) then line else putSortedRow (line++"- ") [] (n+1)
        putSortedRow line ((cell@((col,_),p)):cells) n = 
          if (n >= boardSize) then line
                              else if (n == col) then case p of Hori -> putSortedRow (line++"h ") cells (n+1)
                                                                Vert -> putSortedRow (line++"v ") cells (n+1)
                                                 else putSortedRow (line++"- ") (cell:cells) (n+1)
        getRows n = if n >= boardSize then [] else (cellsAtRow n):(getRows (n+1))
        cellsAtRow r = [ cell | (cell@((col,row),_))<-board, row==r ]
        sortRow row = sortBy sorter row
        sorter ((_,a),_) ((_,b),_) = compare a b
        getLines n = if n >= boardSize then [] else (nSpaces n):(getLines (n+1))
        nSpaces 0 = []
        nSpaces n = " "++(nSpaces (n-1))
    in "Active player is: " ++ (show player) ++ "\n" ++ strBoard
{-
 - - - -
 -  - - -
 -   - - -
 -    - - -
 - -}
