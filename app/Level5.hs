module Level5 where
import Graphics.Gloss
import Interactions
import Screens
import Assets
import WeHateThisGame
import Graphics.Gloss.Interface.IO.Game

drawLv5 :: State a -> Picture
drawLv5 (State theme grid player _ _ _) =
    pictures [background, levelmap grid, player_]
    where
        background = screenBackground theme
        player_ = playerSprite theme player
        levelmap = getLevelMap theme

handleWorld (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state
    
-- | For every other case, world is as is
handleWorld _ state = state

updateWorld :: Float -> State Block -> State Block
updateWorld _ currentState@(State _ _ _ _ _ Over)
    = currentState
updateWorld _ currentState@(State _ _ _ _ _ Paused)
    = currentState
updateWorld _ (State theme grid player state winState gameState) = newState
    where
        newState = State theme grid player' state winState gameState
        player' = getNewState player''
        player'' = movePlayer player grid
        getNewState p = if playerOutOfScreen p then (200, -600, Still, ToDown 0 1)
            else p

-- | Game function
-- data State = State Theme [[Block]] Player _ Bool
game5 :: Theme -> IO()
game5 theme = play window black 90
        (State theme lv5 (200, -600, Still, ToDown 0 1) Empty True Resumed)
        (drawWorld drawLv5) handleWorld updateWorld