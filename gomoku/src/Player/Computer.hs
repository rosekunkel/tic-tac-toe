module Player.Computer (playerComputer) where 

import System.Random (randomRIO)

import Types 

playerComputer :: Player
playerComputer = Player getRandomMove "Computer" 


getRandomMove :: Tile -> Board -> IO (Int, Int)
getRandomMove t b = do
    col <- randomRIO (1,dimM dim)
    row <- randomRIO (1,dimN dim)
    case b??(row, col) of
        EmptyTile -> return (row, col)
        _         -> getRandomMove t b
