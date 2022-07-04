module PlayLevels where 
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game ( Event )
import WeHateThisGame
import Screens
import Level0
import Level5
import Level3
import Level6
import Level4
import Level7
import Level8

-- | General drawlevel function
drawLevel :: State -> Picture
drawLevel state
    = case getLevel state of
        Lv0 -> drawLv0 state
        Lv3 _ -> drawLv3 state
        Lv4 _ -> drawLv4 state
        Lv5 -> drawLv5 state
        Lv6 _ -> drawLv6 state
        Lv7 _ -> drawLv7 state
        Lv8 _ -> drawLv8 state

-- | General level handler
handleWorld :: Event -> State -> State
handleWorld event state
    = case getLevel state of
        Lv0 -> handleWorld0 event state
        Lv3 _ -> handleWorld3 event state
        Lv4 _ -> handleWorld4 event state
        Lv5 -> handleWorld5 event state
        Lv6 _ -> handleWorld6 event state
        Lv7 _ -> handleWorld7 event state
        Lv8 _ -> handleWorld8 event state

-- | General world updater
updateWorld :: Float -> State -> State
updateWorld time state
    = case getLevel state of
        Lv0 -> updateWorld0 time state
        Lv3 _ -> updateWorld3 time state
        Lv4 _ -> updateWorld4 time state
        Lv5 -> updateWorld5 time state
        Lv6 _ -> updateWorld6 time state
        Lv7 _ -> updateWorld7 time state
        Lv8 _ -> updateWorld8 time state

-- | General game runner
game :: IO ()
game = play window black 90
        (State LightTheme lv0 (200, -600, Still, ToDown 0 1) Lv0 True Resumed)
        (drawWorld drawLevel) handleWorld updateWorld