module Screens where

import Graphics.Gloss
import Assets
import WeHateThisGame

window :: Display
window = InWindow "Random window" (1600, 900) (100, 100)

drawColumn :: Theme -> [Block] -> Picture
drawColumn t (col:cols) =
    drawBlock t col <> translate 100 0 (drawColumn t cols)
drawColumn t [] = blank

drawRows :: Theme -> [[Block]] -> Picture
drawRows t (row: rows) =
    drawColumn t row <> translate 0 (-100) (drawRows t rows)
drawRows t [] = blank

getBackground :: Theme -> [[Block]] -> Picture
getBackground t bg = translate (-750) 400 (drawRows t bg)

lv8 :: [[Block]]
lv8 = [allBlock WallBlock 16] ++ map (turnTo borders) [1 .. 6] ++ [numbered] ++ [allBlock WallBlock 16]
    where
        allBlock block n = map (turnTo block) [1 .. n]
        numbered = [WallBlock] ++ map getNumbered [1..14] ++ [WallBlock]
        borders = [WallBlock] ++ allBlock JumpingBlock 14 ++ [WallBlock]
        turnTo item _ = item
        getNumbered item = NumberedBlock (item `mod` 10)