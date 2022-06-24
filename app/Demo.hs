module Demo where

import Graphics.Gloss
import Assets
import WeHateThisGame


runner' theme = display window (getBackgroundColor theme) (pictures [
    wallBlock theme, translate 100 0 (jumpingBlock theme <> portal),
    translate 100 100 (jumpingBlock theme),
    translate (-100) 0 playerSprite,
    translate 0 100 (cage theme),
    translate (-200) 100 (rollingStone theme)
    ])

runner :: IO()
runner = runner' DarkTheme