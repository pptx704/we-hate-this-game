module Level5 where
import Graphics.Gloss
import Interactions
import WeHateThisGame
import Graphics.Gloss.Interface.IO.Game

-- | No level specific graphics
drawLv5 :: State -> Picture
drawLv5 _ = blank

-- | Player can jump while on air
-- rest is same as general
handleWorld5 :: Event -> State -> State
handleWorld5 (EventKey (SpecialKey k) pos sp@(Modifiers shft _ _) _) 
    state@(State theme grid p@(x, y, d, _) s w gs)
    = case (k, pos, shft) of
        (KeySpace, Down, Up) -> State theme grid player' s w gs
        _ -> applyMovement k pos sp state
    where
        player' = case gs of 
            Paused -> p
            _ -> (x, y, d, ToUp 5 0.1)
handleWorld5 _ state = state

-- | Update world is generalized
updateWorld5 :: Float -> State -> State
updateWorld5 _ = updateStates