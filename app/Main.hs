module Main where
import WeHateThisGame
import Level0 (game0)
import Level3 (game3)
import Level4 (game4)
import Level5 (game5)
import Level6 (game6)
import Level7 (game7)
import Level8 (game8)
-- import Assets (getBackgroundColor)
-- import Graphics.Gloss (display)
-- import Screens (getBackground, window, lv6)
-- import WeHateThisGame (Theme (LightTheme, DarkTheme), Block)

main :: IO ()
-- main = game DarkTheme
--main = display window (getBackgroundColor LightTheme) (getBackground LightTheme lv6)
main = game8 DarkTheme