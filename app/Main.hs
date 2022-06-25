module Main where
import Level6 (game)
import Assets (getBackgroundColor)
import Graphics.Gloss (display)
import Screens (getBackground, window, lv6)
import WeHateThisGame (Theme (LightTheme, DarkTheme), Block)

main :: IO ()
main = game DarkTheme
--main = display window (getBackgroundColor LightTheme) (getBackground LightTheme lv6)