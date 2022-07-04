module Level7 where
import Graphics.Gloss
import Interactions
import Screens
import WeHateThisGame
import Graphics.Gloss.Interface.IO.Game

-- | No level specific design
drawLv7 :: State Int -> Picture
drawLv7 _ = blank
        
-- | Handling world is the general one
handleWorld7 :: Event -> State Int -> State Int
handleWorld7 (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state
handleWorld7 _ state = state

-- | Update world needs overwriting some portion of generalized
-- movements because we need to count the deaths
updateWorld7 :: Float -> State Int -> State Int
updateWorld7 _ st@(State theme grid _ state _ gameState) = newState
    where
        newState = State theme grid' player' state' winningState' gameState
        winningState' = state == 5
        player' = fst $ getNewState (player'', state)
        state' = snd $ getNewState (player'', state)
        grid' = changeCell (8, 3) (NumberedBlock state') grid
        player'' = getPlayer (updateStates st)
        getNewState (p, s) = if playerOutOfScreen p then ((200, -600, Still, ToDown 0 1), s+1)
            else (p, s)

-- | Game function
game7 :: Theme -> IO ()
game7 theme = play window black 90
        (State theme lv7 (200, -600, Still, ToDown 0 1) 0 True Resumed)
        (drawWorld drawLv7) handleWorld7 updateWorld7