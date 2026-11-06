data Player = Hori | Vert deriving Eq

type Board = [(Position, Player)]
type Game = (Board, Player, Int)
type Position = (Int, Int)
data Move = Position
type Winner = Maybe Player
