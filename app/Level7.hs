module Level7 where
import Graphics.Gloss
import Interactions
import Screens
import Assets
import WeHateThisGame
import Graphics.Gloss.Interface.IO.Game

drawLv7 :: State Int -> Picture
drawLv7 (State theme grid player _ _ _) =
    pictures [background, levelmap grid, player_]
    where
        background = screenBackground theme
        player_ = playerSprite theme player
        levelmap = getLevelMap theme
        
handleWorld7 :: Event -> State Int -> State Int
handleWorld7 (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state
-- | For every other case, world is as is
handleWorld7 _ state = state

updateWorld7 :: Float -> State Int -> State Int
updateWorld7 _ currentState@(State _ _ _ _ _ Over)
    = currentState
updateWorld7 _ currentState@(State _ _ _ _ _ Paused)
    = currentState
updateWorld7 _ (State theme grid player state _ gameState) = newState
    where
        newState = State theme grid' player' state' winningState' gameState
        winningState' = state == 5
        player' = fst $ getNewState (player'', state)
        state' = snd $ getNewState (player'', state)
        grid' = changeCell (8, 3) (NumberedBlock state') grid
        player'' = movePlayer player grid
        getNewState (p, s) = if playerOutOfScreen p then ((200, -600, Still, ToDown 0 1), s+1)
            else (p, s)

game7 :: Theme -> IO ()
game7 theme = play window black 90
        (State theme lv7 (200, -600, Still, ToDown 0 1) 0 True Resumed)
        (drawWorld drawLv7) handleWorld7 updateWorld7