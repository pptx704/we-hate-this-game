module Main where
import Level6 (game)
import Assets (getBackgroundColor)
import Graphics.Gloss (display)
import Screens (lv8, getBackground, window)
import WeHateThisGame (Theme(LightTheme), Block (WallBlock, JumpingBlock, NumberedBlock))

main :: IO ()
main = display window (getBackgroundColor LightTheme) (getBackground LightTheme lv8)