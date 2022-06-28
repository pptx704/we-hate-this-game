module Screens where

import Graphics.Gloss
import Assets
import WeHateThisGame

-- | Game window 1600 x 900
window :: Display
window = InWindow "Random window" (1600, 900) (100, 100)

-- | Gets the world generated on (0,0) and places it on top corner
drawWorld :: (State a -> Picture) -> State a -> Picture
drawWorld f a = translate (-750) 400 $ f a

-- | The next two functions draw a grid to game map
drawColumn :: Theme -> [Block] -> Picture
drawColumn t (col:cols) =
    drawBlock t col <> translate 100 0 (drawColumn t cols)
drawColumn _ [] = blank

drawRows :: Theme -> [[Block]] -> Picture
drawRows t (row: rows) = 
    drawColumn t row <> translate 0 (-100) (drawRows t rows)
drawRows _ [] = blank

-- | This function is just to get away with the OpenGL `translate` stack limit
getBackground :: Theme -> [[Block]] -> Picture
getBackground = drawRows

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
    map (\_ ->  WallBlock : allBlock Empty 15) [0, 1] ++
    map (\_ -> allBlock WallBlock 16) [0, 1]