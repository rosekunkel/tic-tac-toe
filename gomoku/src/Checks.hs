module Checks where

import Prelude hiding ((!!))
import qualified Data.List 

import Types 

p1wins, p2wins :: Board -> Bool
p1wins b = tileWins b X
p2wins b = tileWins b O

tileWins :: Board -> Tile -> Bool
tileWins b t = 
   any (\col -> any (\row -> all (\k -> b??(row,col+k)   == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col)   == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col+k) == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row-k,col+k) == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim] 


winningMoves :: Board -> Tile -> [Move]
winningMoves b t = 
   concatMap (\col -> concatMap (\row -> if all (\k -> b??(row,col+k)   == t) [0..dimK dim-1] then [(row,col+k)   | k <- [0..dimK dim-1]] else []) [1..dimM dim]) [1..dimN dim] ++
   concatMap (\col -> concatMap (\row -> if all (\k -> b??(row+k,col)   == t) [0..dimK dim-1] then [(row+k,col)   | k <- [0..dimK dim-1]] else []) [1..dimM dim]) [1..dimN dim] ++
   concatMap (\col -> concatMap (\row -> if all (\k -> b??(row+k,col+k) == t) [0..dimK dim-1] then [(row+k,col+k) | k <- [0..dimK dim-1]] else []) [1..dimM dim]) [1..dimN dim] ++
   concatMap (\col -> concatMap (\row -> if all (\k -> b??(row-k,col+k) == t) [0..dimK dim-1] then [(row-k,col+k) | k <- [0..dimK dim-1]] else []) [1..dimM dim]) [1..dimN dim] 


showWinningBoard :: Board -> Tile -> String 
showWinningBoard b t = unlines [Data.List.intercalate "|" row | row <- blist]
  where
     boardAsList b = [[if (x,y) `elem` wins then color (show (b??(x,y))) else show (b??(x,y)) | y <- [1..dimM dim]] | x <- [1..dimN dim]]
     blist         = boardAsList b
     wins          = winningMoves b t 
     color x       = "\x1b[32m" ++ x ++ "\x1b[0m"


showBoardNew :: Move -> Board -> String 
showBoardNew m b = unlines [Data.List.intercalate "|" row | row <- blist]
  where
     boardAsList b = [[if (x,y) == m then color (show (b??(x,y))) else show (b??(x,y)) | y <- [1..dimM dim]] | x <- [1..dimN dim]]
     blist         = boardAsList b
     color x       = "\x1b[31m" ++ x ++ "\x1b[0m"


checkFull :: Board -> Bool
checkFull b = all (\row -> all (\col -> b??(row, col) /= EmptyTile) [1..dimM dim]) [1..dimN dim]


scoreBoard :: Tile -> Board -> Maybe Int 
scoreBoard tile board 
  | tileWins board tile 
  = Just 1
  | tileWins board (flipTile tile)   
  = Just (-1) 
  | checkFull board           
  = Just 0
  | otherwise
  = Nothing 
