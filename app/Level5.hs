module Level5 where
import Graphics.Gloss
import Interactions
import Screens
import Assets
import WeHateThisGame
import Graphics.Gloss.Interface.IO.Game

newtype Lv5Type = Lv5Type Block


drawLv5 :: State Lv5Type -> Picture
drawLv5 (State theme grid player _ _ _) =
    pictures [background, levelmap grid, player_]
    where
        background = screenBackground theme
        player_ = playerSprite theme player
        levelmap = getLevelMap theme

handleWorld5 :: Event -> State Lv5Type -> State Lv5Type
handleWorld5 (EventKey (SpecialKey k) pos sp@(Modifiers shft _ _) _) state@(State theme grid (x, y, d, _) s w gs)
    = case (k, pos, shft) of
        (KeySpace, Down, Up) -> State theme grid player' s w gs
        _ -> applyMovement k pos sp state
    where
        player' = (x, y, d, ToUp 5 0.1)
handleWorld5 _ state = state

updateWorld5 :: Float -> State Lv5Type -> State Lv5Type
updateWorld5 _ (State theme grid player state winningState gs) = newState
    where
        newState = State theme grid player' state winningState gs
        player' = movePlayer player grid

-- | Game function
-- data State = State Theme [[Block]] Player _ Bool
game5 :: Theme -> IO()
game5 theme = play window black 90
        (State theme lv5 (200, -600, Still, ToDown 0 1) (Lv5Type Empty) True Resumed)
        (drawWorld drawLv5) handleWorld5 updateWorld5