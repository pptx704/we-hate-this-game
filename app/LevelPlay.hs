module LevelPlay where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game ( Event )
import WeHateThisGame
import Level0
import Level5
import Screens
import Level7

class GamePlay a where
    handleState :: Event -> State a -> State a
    updateState :: Float -> State a -> State a
    drawState :: State a -> Picture

instance GamePlay Block where
    handleState = handleWorld0
    updateState = updateWorld0
    drawState = drawWorld drawLv0

instance GamePlay Lv5Type where
    handleState = handleWorld5
    updateState = updateWorld5
    drawState = drawWorld drawLv5

instance GamePlay Int where
    handleState = handleWorld7
    updateState = updateWorld7
    drawState = drawWorld drawLv7

-- | Game function
-- data State = State Theme [[Block]] Player _ Bool
game :: Theme -> IO()
game theme = play window black 90 
    -- (State theme lv7 (200, -600, Still, ToDown 0 1) (0::Int) True Resumed)
    (State theme lv5 (200, -600, Still, ToDown 0 1) (Lv5Type Empty) True Resumed)
        drawState handleState updateState