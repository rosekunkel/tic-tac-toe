module Checks where

import Prelude hiding ((!!))

import Types 

p1wins, p2wins :: Board -> Bool
p1wins b = tileWins b X
p2wins b = tileWins b O

tileWins :: Board -> Tile -> Bool
tileWins b t = 
   any (\row -> all (\col -> b!!(row,col ) == t) [1..3]) [1..3] ||
   any (\col -> all (\row -> b!!(row,col) == t) [1..3]) [1..3] ||
   all (\rc -> b!!(rc,rc) == t) [1..3] ||
   all (\rc -> b!!(rc,4-rc) == t) [1..3]

checkFull :: Board -> Bool
checkFull b = all (\row -> all (\col -> b!!(row, col) /= EmptyTile) [1..3]) [1..3]
