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
computeMove tile board = return $ minimax tile (scoredBoardFromBoard board) heuristicFromScoredBoard

data Value = Loss | Draw | Heuristic Int | Win
  deriving (Eq, Ord)

data ScoredMove = ScoredMove
  { getMove :: Move
  , getValue :: Value
  } deriving (Eq)

instance Ord ScoredMove where
  x <= y = getValue x <= getValue y
  
type ScoredBoard = ([(Move, Tile)], Int) --Store the heuristic so that only changes need to be calculated

justIf :: Bool -> a -> Maybe a
justIf True x = Just x
justIf False _ = Nothing

scoreToValue :: Int -> Value
scoreToValue (-1) = Loss
scoreToValue 0 = Draw
scoreToValue 1 = Win

valueToScoredMove :: Value -> ScoredMove
valueToScoredMove v = ScoredMove undefined v

minimax :: Tile -> ScoredBoard -> (Tile -> ScoredBoard -> Int) -> Move
minimax tile board heuristic = getMove $ maximizingMove maxDepth board
  where
    maybeScore board depth =
      justIf (depth <= 0) (Heuristic (heuristic tile board)) <|>
      scoreToValue <$> scoreBoard tile (fst board)
    maximizingMove depth board = fromMaybe
      (maximum $
       (\move -> ScoredMove move (getValue $
                                  minimizingMove (depth - 1) $
                                  putScored board tile move)) <$>
       validMoves (fst board))
      (valueToScoredMove <$> maybeScore board depth)
    minimizingMove depth board = fromMaybe
      (minimum $
       (\move -> ScoredMove move (getValue $
                                  maximizingMove (depth - 1) $
                                  putScored board (flipTile tile) move)) <$>
       validMoves (fst board))
      (valueToScoredMove <$> maybeScore board depth)

heuristicFromBoard :: Tile -> Board -> Int
heuristicFromBoard tile board = (valueBoard tile board) - (valueBoard (flipTile tile) board)
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
      
heuristicFromScoredBoard :: Tile -> ScoredBoard -> Int
heuristicFromScoredBoard tile (board, score) = case tile of
  X -> score
  O -> -score
  _ -> 0

putScored :: ScoredBoard -> Tile -> Move -> ScoredBoard
putScored (board, score) tile move = (put board tile move, score + scoreChange board tile move)

-- Returns the score change in X's perpective if the given tile is placed on the given move
scoreChange :: Board -> Tile -> Move -> Int
scoreChange board placedTile move = (valueBoardChange X board) - (valueBoardChange O board)
  where
    valueBoardChange scoreTile board = foldl (+) 0 (map (valueLineChange scoreTile board) (restrictedLines move))
    valueLineChange scoreTile board moves
      | placedTile == scoreTile = (evaluateCount (count+1)) - (evaluateCount count)
      | otherwise = -(evaluateCount count)
          where count = foldl (+) 0 (map (valueMove scoreTile board) moves)
    valueMove scoreTile board move = case (board ?? move) of
      EmptyTile -> 0
      currentTile -> if currentTile == scoreTile then 1 else -dimK dim
    evaluateCount count
      | count >= dimK dim = 9999999999
      | count > 0 = (2 ^ count) -- Arbitrary triangular number sequence of scores.
      | otherwise = 0

restrictedLines :: Move -> [[Move]]
restrictedLines (fixedRow, fixedCol) = getRows ++ getColumns ++ getDiagonals1 ++ getDiagonals2
  where
    getRows = [[(fixedRow, col+k) | k <- [0..k-1]] | col <- [max (fixedCol-k+1) 1 .. min fixedCol (n-k+1)]]
    getColumns = [[(row+k, fixedCol) | k <- [0..k-1]] | row <- [max (fixedRow-k+1) 1 .. min fixedRow (m-k+1)]]
    getDiagonals1 = [[(fixedRow+disp+k, fixedCol+disp+k) | k <- [0..k-1]] | disp <- [maximum [-k+1, -fixedRow+1, -fixedCol+1] .. minimum [0, m-fixedRow-k+1, n-fixedCol-k+1]]]
    getDiagonals2 = [[(fixedRow+disp+k, fixedCol-disp-k) | k <- [0..k-1]] | disp <- [maximum [-k+1, -fixedRow+1, -n+fixedCol] .. minimum [0, m-fixedRow-k+1, fixedCol-k]]]
    k = dimK dim
    m = dimM dim
    n = dimN dim
      
allLines :: [[Move]]
allLines = getRows ++ getColumns ++ getDiagonals1 ++ getDiagonals2
  where
    getRows = [[(row, col+k) | k <- [0..dimK dim - 1]] | row <- [1..dimM dim], col <- [1..dimN dim - dimK dim + 1]]
    getColumns = [[(row+k, col) | k <- [0..dimK dim - 1]] | row <- [1..dimM dim - dimK dim + 1], col <- [1..dimN dim]]
    getDiagonals1 = [[(row+k, col+k) | k <- [0..dimK dim - 1]] | row <- [1..dimM dim - dimK dim + 1], col <- [1..dimN dim - dimK dim + 1]]
    getDiagonals2 = [[(row+k, col-k) | k <- [0..dimK dim - 1]] | row <- [1..dimM dim - dimK dim + 1], col <- [dimK dim..dimN dim]]

scoredBoardFromBoard :: Board -> ScoredBoard
scoredBoardFromBoard board = (board, heuristicFromBoard X board)


