module Main where

import WeHateThisGame ( Theme(DarkTheme, LightTheme) )
import Level3 (game3)
import Level0 (game0)
-- import Assets (getBackgroundColor)
-- import Graphics.Gloss (display)
-- import Screens (getBackground, window, lv6)
-- import WeHateThisGame (Theme (LightTheme, DarkTheme), Block)

main :: IO ()
-- main = game DarkTheme
--main = display window (getBackgroundColor LightTheme) (getBackground LightTheme lv6)
main = game3 DarkTheme