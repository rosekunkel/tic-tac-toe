module Main where 

import Control.Monad

import Types 
import Checks 
import Misc 

import Player.Human    (playerHuman   )
import Player.Computer (playerComputer)

player1, player2 :: Player
player1 = playerComputer    
player2 = playerHuman "Name" 

main :: IO ()
main = do
    putStrLn "This is classic tic tac toe game."
    rounds <- prompt "How many rounds should we play?"
    [(p1,i1),(p2,i2)] <- playRounds (read rounds) player1 player2 
    if i1 == i2 
      then putStrLn "Its a tie!" 
      else putStrLn ("The winner is " ++ show (if i1 < i2 then p2 else p1))


playRounds :: Int -> Player -> Player -> IO Score
playRounds rounds player1 player2 = 
  foldM (playRound pi1 pi2) [(pi1,0),(pi2,0)] [1..rounds]
  where 
    pi1 = PI player1 X 1 
    pi2 = PI player2 O 2 

playRound :: PlayerInfo -> PlayerInfo -> Score -> Int -> IO Score 
playRound p1 p2 score i = do 
   putStrLn ("Score:: " ++ showScore score)
   putStrLn ("Round " ++ show i ++ "!")
   putStrLn ((if (i `mod` 2 == 0) then show p2 else show p1)  ++ " plays first")
   result <- if (i `mod` 2 == 0) then play p2 p1 emptyBoard else play p1 p2 emptyBoard
   case result of 
      Just p  -> putStrLn (show p ++ " wins!\n\n") >> return (incr p score)
      Nothing -> putStrLn "Its a tie!\n\n" >> return score 


play :: PlayerInfo -> PlayerInfo -> Board -> IO (Maybe PlayerInfo)
play pi1@(PI p1 t1 _) pi2 board = do 
  move <- playerMove p1 board
  case put board t1 move of
    Nothing -> putStrLn "Invalid move." >> return (Just pi2)
    Just b  -> do putStrLn $ showBoard b
                  if tileWins b t1
                    then return (Just pi1) 
                    else if checkFull b 
                    then return Nothing 
                    else play pi2 pi1 b 
