module Level5 where
import Graphics.Gloss
import Interactions
import Screens
import WeHateThisGame
import Graphics.Gloss.Interface.IO.Game

-- | This was used to differentiate Lv5 and Lv0 while trying to connect levels
-- that attempt was futile
newtype Lv5Type = Lv5Type Block

-- | No level specific graphics
drawLv5 :: State Lv5Type -> Picture
drawLv5 _ = blank

-- | Player can jump while on air
-- rest is same as general
handleWorld5 :: Event -> State Lv5Type -> State Lv5Type
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
updateWorld5 :: Float -> State Lv5Type -> State Lv5Type
updateWorld5 _ = updateStates

-- | Game function
-- data State = State Theme [[Block]] Player _ Bool
game5 :: Theme -> IO()
game5 theme = play window black 90
        (State theme lv5 (200, -600, Still, ToDown 0 1) (Lv5Type Empty) True Resumed)
        (drawWorld drawLv5) handleWorld5 updateWorld5
