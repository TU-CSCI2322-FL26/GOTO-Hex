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

-- testing code --
testGame = (testBoard, Hori, 11)
testBoard = [ ((x,y), if mod x 2 == 0 then Hori else Vert) | x<-[0..10], y<-[0..10] ]
-- testing code --

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



showGame (board, player, boardSize) = 
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