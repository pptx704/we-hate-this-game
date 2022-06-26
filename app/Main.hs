module Main where
import Level6
import WeHateThisGame
import Interactions
import Screens (lv6)
-- import Assets (getBackgroundColor)
-- import Graphics.Gloss (display)
-- import Screens (getBackground, window, lv6)
-- import WeHateThisGame (Theme (LightTheme, DarkTheme), Block)

main :: IO ()
main = game DarkTheme
--main = display window (getBackgroundColor LightTheme) (getBackground LightTheme lv6)
--main = do
--    print $ changeCell (0, 2) (const (NumberedBlock 3)) lv6