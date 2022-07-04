module Level0 where
import Graphics.Gloss
import Interactions
import Assets
import WeHateThisGame
import Graphics.Gloss.Interface.IO.Game

-- | No level specific drawing
drawLv0 :: State -> Picture
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
handleWorld0 :: Event -> State -> State
handleWorld0 (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state
handleWorld0 _ state = state

-- | Update world is also basic
updateWorld0 :: Float -> State -> State
updateWorld0 _ = updateStates