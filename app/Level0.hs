module Level0 where
import Graphics.Gloss
import Interactions
import Screens
import WeHateThisGame
import Graphics.Gloss.Interface.IO.Game

-- | No level specific drawing
drawLv0 :: State Block -> Picture
drawLv0 _ = blank

-- | Handleworld is basic
handleWorld :: Event -> State Block -> State Block
handleWorld (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state
handleWorld _ state = state

-- | Update world is also basic
updateWorld :: Float -> State Block -> State Block
updateWorld _ = updateStates

-- | game function
game0 :: Theme -> IO()
game0 theme = play window black 90
        (State theme lv0 (200, -600, Still, ToDown 0 1) Empty True Resumed)
        (drawWorld drawLv0) handleWorld updateWorld