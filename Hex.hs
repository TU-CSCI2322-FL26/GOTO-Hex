import Debug.Trace
import Data.Maybe

data Player = Hori | Vert deriving (Show, Eq)
type Board = [Cell]
type Cell = (Position, Player)
type Game = (Board, Player, Integer)
type Position = (Integer, Integer)
data Move = Position
type Winner = Maybe Player



-- testing code --
testGame = (testBoard, Hori, 11)
testBoard = [ ((x,y), if mod x 2 == 0 then Hori else Vert) | x<-[0..10], y<-[0..10] ]
-- testing code --



-- this function takes in a position and a gamestate
-- then returns a list of all cells adjacent to the given position
adjacents :: Game -> Cell -> [Cell]
adjacents (board,_,max) ((x,y),_) = catMaybes $ map (getCell board) possibles
  where possibles = (x-1,y):(x+1,y):(x,y-1):(x+1,y-1):(x-1,y+1):(x,y+1):[]
        foldHelper _ [] = []
        foldHelper pos lst = case getCell board pos of Nothing -> lst
                                                       Just n -> n:lst

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
