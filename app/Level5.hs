module Level5 where
import Graphics.Gloss
import Interactions
import Screens
import Assets
import WeHateThisGame
import Graphics.Gloss.Interface.IO.Game


drawLv5 :: State a -> Picture
drawLv5 (State theme grid player _ _) =
    pictures [background, levelmap grid, player_]
    where
        background = screenBackground theme
        player_ = playerSprite theme player
        levelmap = getLevelMap theme

handleWorld :: Event -> State Block -> State Block
handleWorld (EventKey (SpecialKey k) pos sp _) state@(State theme grid (x, y, d, _) s w)
    = case (k, pos) of
        (KeySpace, Down) -> State theme grid player' s w
        _ -> applyMovement k pos sp state
    where
        player' = (x, y, d, ToUp 5 0.1)
handleWorld _ state = state

updateWorld :: Float -> State Block -> State Block
updateWorld _ (State theme grid player state winningState) = newState
    where
        newState = State theme grid player' state winningState
        player' = movePlayer player grid

-- | Game function
-- data State = State Theme [[Block]] Player _ Bool
game5 :: Theme -> IO()
game5 theme = play window black 90
        (State theme lv5 (200, -600, Still, ToDown 0 1) Empty True)
        (drawWorld drawLv5) handleWorld updateWorld