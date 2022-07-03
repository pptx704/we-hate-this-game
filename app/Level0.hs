module Level0 where
import Graphics.Gloss
import Interactions
import Screens
import Assets
import WeHateThisGame
import Graphics.Gloss.Interface.IO.Game


drawLv0 :: State a -> Picture
drawLv0 (State theme grid player _ _) =
    pictures [background, levelmap grid, player_]
    where
        background = screenBackground theme
        player_ = playerSprite theme player
        levelmap = getLevelMap theme

handleWorld :: Event -> State Block -> State Block

handleWorld (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state
-- | For every other case, world is as is
handleWorld _ state = state

updateWorld :: Float -> State Block -> State Block
updateWorld _ (State theme grid player state winningState) = newState
    where
        newState = State theme grid player' state winningState
        player' = movePlayer player grid

-- | Game function
-- data State = State Theme [[Block]] Player _ Bool
game0 :: Theme -> IO()
game0 theme = play window black 90
        (State theme lv0 (200, -600, Still, ToDown 0 1) Empty True)
        (drawWorld drawLv0) handleWorld updateWorld
