module Main where
import System.IO

import Hex
import HexIO
import HexSolver

main :: IO ()
main =
  do putStr "Enter a filepath: "
     hFlush stdout
     filePath <- getLine
     game <- loadGame filePath
     putBestMove game--putStrLn $ showGamePretty game
     --putStrLn $ showGame game

writeGame :: Game -> FilePath -> IO ()
writeGame game location = writeFile location $ showGame game

loadGame :: FilePath -> IO Game
loadGame location = fmap readGame $ readFile location

putBestMove :: Game -> IO ()
putBestMove game = putStr move
  where move = case bestMove game of Just m -> show m
                                     Nothing -> ""
