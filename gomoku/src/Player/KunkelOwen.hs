module Player.KunkelOwen (playerKunkelOwen) where

import Control.Applicative
import Data.Maybe
import Debug.Trace

import Types
import Checks

playerKunkelOwen :: Player
playerKunkelOwen = Player computeMove "KunkelOwen"

maxDepth :: Int
maxDepth = 2

computeMove :: Tile -> Board -> IO Move
computeMove tile board = return $ minimax tile board (\_ _ -> 0)

data Value = Loss | Draw | Heuristic Int | Win
  deriving (Eq, Ord)

data ScoredMove = ScoredMove
  { getMove :: Move
  , getValue :: Value
  } deriving (Eq)

instance Ord ScoredMove where
  x <= y = getValue x <= getValue y

justIf :: Bool -> a -> Maybe a
justIf True x = Just x
justIf False _ = Nothing

scoreToValue :: Int -> Value
scoreToValue (-1) = Loss
scoreToValue 0 = Draw
scoreToValue 1 = Win

valueToScoredMove :: Value -> ScoredMove
valueToScoredMove v = ScoredMove undefined v

minimax :: Tile -> Board -> (Tile -> Board -> Int) -> Move
minimax tile board heuristic = getMove $ maximizingMove maxDepth board
  where
    maybeScore board depth =
      justIf (depth <= 0) (Heuristic (heuristic tile board)) <|>
      scoreToValue <$> scoreBoard tile board
    maximizingMove depth board = fromMaybe
      (maximum $
       (\move -> ScoredMove move (getValue $
                                  minimizingMove (depth - 1) $
                                  put board tile move)) <$>
       validMoves board)
      (valueToScoredMove <$> maybeScore board depth)
    minimizingMove depth board = fromMaybe
      (minimum $
       (\move -> ScoredMove move (getValue $
                                  maximizingMove (depth - 1) $
                                  put board (flipTile tile) move)) <$>
       validMoves board)
      (valueToScoredMove <$> maybeScore board depth)
