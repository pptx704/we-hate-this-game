module Screens where

import Graphics.Gloss
import Assets
import WeHateThisGame

window :: Display
window = InWindow "Random window" (1600, 900) (100, 100)

drawColumn :: Theme -> [Block] -> Picture
drawColumn t (col:cols) =
    drawBlock t col <> translate 100 0 (drawColumn t cols)
drawColumn _ [] = blank

drawRows :: Theme -> [[Block]] -> Picture
drawRows t (row: rows) = 
    drawColumn t row <> translate 0 (-100) (drawRows t rows)
drawRows _ [] = blank

getBackground :: Theme -> [[Block]] -> Picture
getBackground t bg = translate (-750) 400 (drawRows t bg)

-- helpers for level mapping

-- makes N number of blocks of same type
allBlock :: Block -> Int -> [Block]
allBlock block n = map (const block) [1 .. n]

-- Make a bordered row where leftmost and rightmost are wall and rest are empty
borders :: [Block]
borders = [WallBlock] ++ allBlock Empty 14 ++ [WallBlock]

-- Map for level 6
lv6 :: [[Block]]
lv6 =
    [allBlock WallBlock 16] ++
    map (const borders) [1..4] ++
    map (\_ -> allBlock Empty 16) [0, 1] ++
    map (\_ -> allBlock WallBlock 16) [0, 1]