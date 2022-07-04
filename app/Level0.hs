module Level0 where
import Graphics.Gloss
import Interactions
import Screens
import Assets
import WeHateThisGame
import Graphics.Gloss.Interface.IO.Game

drawLv0 :: State Block -> Picture
drawLv0 (State theme grid player _ _ gs) =
    case gs of
        Over -> pictures [background, levelmap grid, player_, gameOver]
        _ -> pictures [background, levelmap grid, player_]
    where
        background = screenBackground theme
        player_ = playerSprite theme player
        levelmap = getLevelMap theme
        gameOver = translate 200 (-400) $ Text "Game Over"

handleWorld0 :: Event -> State Block -> State Block
handleWorld0 (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state
handleWorld0 _ state = state

updateWorld0 :: Float -> State Block -> State Block
updateWorld0 _ currentState = newState
    where
        newState = updateStates currentState

-- game0 :: Theme -> IO()
-- game0 theme = play window black 90
--         (State theme lv0 (200, -600, Still, ToDown 0 1) Empty True Resumed)
--         (drawWorld drawLv0) handleWorld updateWorld