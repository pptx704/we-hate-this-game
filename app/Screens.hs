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

-- | Gets the theme background
screenBackground :: Theme -> Picture
screenBackground t = translate 750 (-400)
    $ color (getBackgroundColor t) $ rectangleSolid 1600 900

-- | The following two functions draw the game background map
drawColumn :: Theme -> [Block] -> Picture
drawColumn t = foldr func blank
    where
        func a b = translate 100 0 b <> drawBlock t a

drawRows :: Theme -> [[Block]] -> Picture
drawRows t = foldr func blank
    where
        func a b = translate 0 (-100) b <> drawColumn t a

-- | This function is just to get away with the OpenGL `translate` stack limit
getLevelMap :: Theme -> [[Block]] -> Picture
getLevelMap = drawRows


-- | helpers for level mapping
-- makes N number of blocks of same type
allBlock :: Block -> Int -> [Block]
allBlock block n = map (const block) [1 .. n]

-- | Make a bordered row where leftmost and rightmost are wall and rest are empty
borders :: [Block]
borders = [WallBlock] ++ allBlock Empty 14 ++ [WallBlock]

-- | Map for level 6
lv6 :: [[Block]]
lv6 =
    [allBlock WallBlock 16] ++
    map (const borders) [1..4] ++
    map (\_ ->  WallBlock : allBlock Empty 15) [0, 1] ++
    map (\_ -> allBlock WallBlock 16) [0, 1]

-- | Map generator for level 8
-- Needs game states to draw the map
lv8 :: ([Int], [Int]) -> [[Block]]
lv8 (usr, bull) =
    [allBlock WallBlock 16, borders, bullrow, borders, usrrow]
    ++ map (const borders) [0, 1] ++
    map (\_ -> allBlock WallBlock 16) [0, 1]

    where
        bullrow = [WallBlock] ++ allBlock Empty 6
            ++ map NumberedBlock bull
            ++ allBlock Empty 6 ++ [WallBlock]
        usrrow = [WallBlock] ++ allBlock Empty 5
            ++ map NumberedBlock usr
            ++ allBlock Empty 5 ++ [WallBlock]

-- | Level 8 initial map
lv8' :: [[Block]]
lv8' = lv8 ([0,0,0,0], [0,0])