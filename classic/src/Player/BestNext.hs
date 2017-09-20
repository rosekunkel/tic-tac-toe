-- | Player that pays the best next move

module Player.BestNext (playerBestNext) where 

import Types  (Player(..), Tile, Board, Move, validMoves, put)
import Checks (scoreBoard)

playerBestNext :: Player 
playerBestNext = Player strategy "Best Next"


strategy :: Tile -> Board -> IO Move
strategy tile board = return $ snd $ maximum scoredMoves 
  where
    scoredMoves = zip scores moves
    scores      = map (evaluateBoard tile . put board tile) moves 
    moves       = validMoves board


evaluateBoard :: Tile -> Board -> Int
evaluateBoard tile board  
  | Just i <- scoreBoard tile board
  =  i
  | otherwise
  = -2
