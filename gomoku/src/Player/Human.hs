module Player.Human (playerHuman) where 

import Types 
import Misc 

playerHuman :: Player 
playerHuman = Player humanMove "Human"
  where
    humanMove _ _ = read <$> prompt "Write your move! (i,j) with 1<=i,j<=3 "
