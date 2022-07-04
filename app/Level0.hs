module Level0 where
import Graphics.Gloss
import Interactions
import Screens
import Assets
import WeHateThisGame
import Graphics.Gloss.Interface.IO.Game

-- | No level specific drawing
drawLv0 :: State Block -> Picture
drawLv0 (State t _ _ _ _ gs) = case gs of
    Resumed -> color (getForegroundColor t) 
        $ pictures [themeChange, pause, resume, move]
    _ -> blank
    where
        themeChange = translate 300 (-200) $
            scaledText "Press Shift + Space to Change Theme"
        pause = translate 300 (-300) $
            scaledText "Press Shift + UpArrow to Pause"
        resume = translate 300 (-400) $
            scaledText "Press Shift + DownArrow to Resume"
        move = translate 300 (-500) $
            scaledText "Use Arrow Keys to Move and Space to Jump"
        scaledText = scale 0.3 0.3 . Text
        
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