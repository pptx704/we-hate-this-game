module Main where
import Level6
import Level8
import WeHateThisGame
-- import Assets (getBackgroundColor)
-- import Graphics.Gloss (display)
-- import Screens (getBackground, window, lv6)
-- import WeHateThisGame (Theme (LightTheme, DarkTheme), Block)

main :: IO ()
-- main = game DarkTheme
--main = display window (getBackgroundColor LightTheme) (getBackground LightTheme lv6)
main = game8 DarkTheme
