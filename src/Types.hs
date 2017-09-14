module Types where 

import Prelude hiding ((!!))

import qualified Data.List 
import qualified Data.Maybe as M 

-------------------------------------------------------------------------------
--- Board ---------------------------------------------------------------------
-------------------------------------------------------------------------------


data Tile = EmptyTile | X | O 
  deriving (Eq)

type Move   = (Int,Int)

type Board  = [(Move, Tile)] 

(!!) :: Board -> Move -> Tile
b!!ij = M.fromMaybe EmptyTile (lookup ij b) 

emptyBoard :: Board
emptyBoard = [((x,y), EmptyTile) | x <- [1..3], y <- [1..3]]

put :: Board -> Tile -> (Int,Int) -> Maybe Board
put b t xy = case b!!xy of
               EmptyTile -> Just $ map (\(ij,tij) -> if ij == xy then (ij,t) else (ij,tij)) b 
               _         -> Nothing

-------------------------------------------------------------------------------
--- Player --------------------------------------------------------------------
-------------------------------------------------------------------------------

data Player = 
  Player { playerMove :: Board -> IO Move
         , playerName :: String 
         }



-------------------------------------------------------------------------------
--- Score ---------------------------------------------------------------------
-------------------------------------------------------------------------------

type Score = [(PlayerInfo, Int)]

showScore [(p1,i1),(p2,i2)] 
  = show p1 ++ " : " ++ show i1 ++ " VS. " ++ show p2 ++ " : " ++ show i2 
showScore _ 
  = ""

incr :: PlayerInfo -> Score -> Score
incr pi xs = map (\(pj,sj) -> if pi == pj then (pj,sj+1) else (pj,sj)) xs 

-------------------------------------------------------------------------------
--- Player Info ---------------------------------------------------------------
-------------------------------------------------------------------------------

data PlayerInfo =  
  PI { playerInfoPlayer :: Player
     , playerInfoTile   :: Tile
     , playerInfoInt    :: Int
     }

instance Eq PlayerInfo where
    p1 == p2 = playerInfoInt p1 == playerInfoInt p2 

instance Show PlayerInfo where
  show pi 
    | pname /= "Computer" && pname /= "Human"
    =  pname 
    | otherwise 
    = "Player " ++ show (playerInfoInt pi) 
    where pname = playerName $ playerInfoPlayer pi



showTile :: Tile -> String
showTile EmptyTile = "     "
showTile X         = "  X  "
showTile O         = "  O  "

showBoard :: Board -> String
showBoard b = let blist = boardAsList b
              in  unlines [Data.List.intercalate "|" row | row <- blist]
              where
                boardAsList b = [[showTile (b!!(x,y)) | y <- [1,2,3]] | x <- [1,2,3]]

showTileNumbers :: String
showTileNumbers  = (unlines
                   [Data.List.intercalate "|" ["(" ++ show x ++ "," ++ show y ++ ")" |
                   y <- [1,2,3]] | x <- [1,2,3]])
