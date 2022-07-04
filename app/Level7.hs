module Level7 where
import Graphics.Gloss
import Interactions
import WeHateThisGame
import Graphics.Gloss.Interface.IO.Game

-- | No level specific design
drawLv7 :: State -> Picture
drawLv7 _ = blank
        
-- | Handling world is the general one
handleWorld7 :: Event -> State -> State
handleWorld7 (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state
handleWorld7 _ state = state

-- | Update world needs overwriting some portion of generalized
-- movements because we need to count the deaths
updateWorld7 :: Float -> State -> State
updateWorld7 _ st@(State _ _ _ (Lv7 _) _ Completed) = updateStates st
updateWorld7 _ st@(State theme grid _ (Lv7 state) winningState gameState) = newState
    where
        newState = State theme grid player' (Lv7 state') winningState gameState'
        gameState' = if state >= 5 then Completed else gameState
        player' = fst $ getNewState (player'', state)
        state' = snd $ getNewState (player'', state)
        player'' = getPlayer (updateStates st)
        getNewState (p, s) = if playerOutOfScreen p then ((200, -600, Still, ToDown 0 1), s+1)
            else (p, s)
updateWorld7 _ s = updateStates s