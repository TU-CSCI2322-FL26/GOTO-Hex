module HexSolver where
import Hex

-- whoWillWin takes a game and returns the player who should win the game, assuming both players play to win
whoWillWin :: Game -> Player
whoWillWin game@(_, player, _) = 
    case getWinner game of 
        Just Hori -> Hori
        Just Vert -> Vert
        Nothing -> 
            let moves = legalMoves game
                nextGames = map (makeMove game) moves 
                results = [whoWillWin g | Just g <- nextGames]
            in case player of
                Hori ->
                    if Hori `elem` results then Hori else Vert
                Vert ->
                    if Vert `elem` results then Vert else Hori
                    

-- bestMove takes a game and returns a move that can win the game
-- always returns a move if legal moves exist (even if losing)
-- returns Nothing if no legal moves exist
bestMove :: Game -> Maybe Move
bestMove game@(_, player, _) = 
    let moves = legalMoves game
        winningMoves =
            [move | move <- moves, 
                    Just nextGame <- [makeMove game move], 
                    whoWillWin nextGame == player]
    in case winningMoves of
        (move:_) -> Just move
        []       -> case moves of
                        (move:_) -> Just move
                        []       -> Nothing
