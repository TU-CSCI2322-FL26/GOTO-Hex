module HexIO where
import Data.List
import Data.List.Split
import Text.Read
import Debug.Trace

import Hex

readGame :: String -> Game
readGame input = (board,player,boardSize)
  where (l1,l2,l3) = case lines input of (a:b:c:_) -> (a,b,c)
                                         _ -> error "invalid Game file - not enough lines"
        (player,boardSize) = case splitOn "," l1 of (pl:bs:_) -> (readPlayer pl, readInteger bs)
                                                    _ -> error "invalid Game file - error on parsing header"
        horiMoves = map (\pos -> (pos,Hori)) (getMoves ( splitOn "," l2))
        vertMoves = map (\pos -> (pos,Vert)) (getMoves ( splitOn "," l3))
        board = horiMoves ++ vertMoves
        readInteger n = case readMaybe n :: Maybe Integer of Nothing -> error "invalid Game file - not a number"
                                                             Just x -> x
        readPlayer "Hori" = Hori
        readPlayer "Vert" = Vert
        readPlayer _ = error "invalid Game file - error parsing player name; should be \"Hori\" or \"Vert\""
        getMoves [] = []
        getMoves ("":rest) = getMoves rest
        getMoves (curr:rest) = case words curr of (x:y:[]) -> (readInteger x, readInteger y):(getMoves rest)
                                                  n -> error "invalid Game file - error on parsing moves"

showGame :: Game -> String
showGame (board,active,boardSize) = line1 ++ (('\n':line2) ++ ('\n':line3)) ++ "\n"
  where line1 = (show active) ++ (',':(show boardSize))
        line2 = intercalate "," lst2
        line3 = intercalate "," lst3
        (lst2,lst3) = foldr sorter ([],[]) board
        sorter :: Cell -> ([String],[String]) -> ([String],[String])
        sorter ((x,y),player) (h,v) = case player of Hori -> (((show x) ++ " " ++ (show y)):h,v)
                                                     Vert -> (h,((show x) ++ " " ++ (show y)):v)


showGamePretty :: Game -> String
showGamePretty (board, player, boardSize) = 
    let strBoard = unlines endLines
        endLines = map (\(l,r) -> showSortedRow l r 0) (zip linePaddings rows)
        rows = [ cellsAtRow n | n<-[0..(boardSize-1)] ]
        linePaddings = [ replicate (fromIntegral n) ' ' | n<-[0..(boardSize-1)] ]
        showCell (_,p) = case p of Hori -> "h "
                                   Vert -> "v "
        showSortedRow line [] n = if (n >= boardSize) then line else showSortedRow (line++"- ") [] (n+1)
        showSortedRow line ((cell@((col,_),p)):cells) n = 
          if (n >= boardSize) then line
                              else if (n == col) then showSortedRow (line++(showCell cell)) cells (n+1)
                                                 else showSortedRow (line++"- ") (cell:cells) (n+1)
        cellsAtRow r = sortBy sorter [ cell | (cell@((col,row),_))<-board, row==r ]
        sorter ((a,_),_) ((b,_),_) = compare a b
    in "Active player is: " ++ (show player) ++ "\n" ++ strBoard
