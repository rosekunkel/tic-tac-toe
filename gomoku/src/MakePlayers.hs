module Main where 

import System.IO
import System.Process
import Data.List 

main :: IO ()
main = do 
  files <- readProcess "ls" ["Player"] []
  let players = (takeWhile (/='.')) <$> lines files  
  putStrLn ("Players registerd:\n\n" ++ unlines players)
  writeFile "Players.hs" (preamble ++ unlines (imported <$> players) ++ allPlayers (makePlayerName <$> players))

imported :: String -> String
imported name = "import Player." ++ name ++ " (player" ++ name ++ ")"

makePlayerName :: String -> String 
makePlayerName name = "(\"" ++ name ++ "\", player" ++ name ++ ")"

allPlayers :: [String] -> String 
allPlayers xs = unlines (
  ["players :: [(String, Player)]"
  , "players = ["
  , "  " ++ concat (intersperse "," xs)
  , "  ]"] )

preamble :: String 
preamble = unlines 
  [ "module Players (players) where"
  , "import Types"
  ]
