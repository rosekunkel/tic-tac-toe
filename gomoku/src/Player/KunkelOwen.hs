module Player.KunkelOwen (playerKunkelOwen) where

import Control.Applicative
import Data.Maybe
import Debug.Trace

import Types
import Checks

playerKunkelOwen :: Player
playerKunkelOwen = Player computeMove "KunkelOwen"

maxDepth :: Int
maxDepth = 1

computeMove :: Tile -> Board -> IO Move
computeMove tile board = return $ minimax tile board testHeuristic

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

testHeuristic :: Tile -> Board -> Int
testHeuristic tile board = (valueBoard tile board) - (valueBoard (flipTile tile) board)
  where
    valueBoard tile board = foldl (+) 0 (map (valueLine tile board) allLines)
    valueLine tile board moves = evaluateCount $
      foldl (+) 0 (map (valueMove tile board) moves)
    valueMove tile board move = case (board ?? move) of
      EmptyTile -> 0
      currentTile -> if currentTile == tile then 1 else -dimK dim
    evaluateCount count
      | count > 0 = (count * (count+1)) -- Arbitrary triangular number sequence of scores.
      | otherwise = 0
      
allLines :: [[Move]]
allLines = getRows ++ getColumns ++ getDiagonals1 ++ getDiagonals2
  where
    getRows = [[(row,col+k) | k <- [0..dimK dim - 1]] | row <- [1..dimM dim], col <- [1..dimN dim - dimK dim + 1]]
    getColumns = [[(row+k,col) | k <- [0..dimK dim - 1]] | row <- [1..dimM dim - dimK dim + 1], col <- [1..dimN dim]]
    getDiagonals1 = [[(row+k,col+k) | k <- [0..dimK dim - 1]] | row <- [1..dimM dim - dimK dim + 1], col <- [1..dimN dim - dimK dim + 1]]
    getDiagonals2 = [[(row+k,col-k) | k <- [0..dimK dim - 1]] | row <- [1..dimM dim - dimK dim + 1], col <- [dimK dim..dimN dim]]




