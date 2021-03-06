module Screens where

import Graphics.Gloss
import Assets
import WeHateThisGame

-- | Game window 1600 x 900
window :: Display
window = InWindow "We Hate This Game" (1600, 900) (10, 10)

-- | Generates common world, embedds level specific world to it and then
-- puts the drawing to topleft corner
drawWorld :: (State -> Picture) -> State -> Picture
drawWorld drawFunc state@(State theme grid player _ _ gameState) 
    = translate (-750) 400 $
    pictures [background, levelmap grid, player', drawFunc state, message]
    where
        message = color (getForegroundColor theme) message'
        message' = case gameState of
            Over -> translate 450 (-400) $ Text "Game Over"
            Paused -> translate 450 (-400) $ Text "Paused"
            Completed -> translate 450 (-400) $ Text "You Win!"
            _ -> blank
        background = screenBackground theme
        player' = playerSprite theme player
        levelmap = getLevelMap theme

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


-- | The holed rows
holed :: [Block]
holed = allBlock WallBlock 4 ++ allBlock Empty 8 
            ++ allBlock WallBlock 4

-- | Map for level 0
lv0 :: [[Block]]
lv0 = [allBlock WallBlock 16] ++
            map (const borders) [1..4::Int] ++
            map (\_ ->  WallBlock : allBlock Empty 15) [0, 1::Int] ++
            map (const smallHole) [0,1::Int]
    where
        smallHole = allBlock WallBlock 7 ++ allBlock Empty 2
            ++ allBlock WallBlock 7

-- | Map for level 6
lv6 :: [[Block]]
lv6 = [allBlock WallBlock 16] ++
    map (const borders) [1..4::Int] ++
    map (\_ ->  WallBlock : allBlock Empty 15) [0, 1::Int] ++
    map (\_ -> allBlock WallBlock 16) [0, 1::Int]

-- | Map generator for level 8
-- Needs game states to draw the map
lv8 :: ([Int], [Int]) -> [[Block]]
lv8 (usr, bull) =
    [allBlock WallBlock 16, borders, bullrow, borders, usrrow]
    ++ map (const borders) [0,1::Int] ++
    map (\_ -> allBlock WallBlock 16) [0, 1::Int]

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


-- | Level 3 map
lv3 :: [[Block]]
lv3 = [allBlock WallBlock 16] 
            ++ map (const borders) [1..6::Int] 
            ++ map (const holed) [0, 1::Int]

-- | Level 5
lv5 :: [[Block]]
lv5 = [allBlock WallBlock 16] ++
     map (const borders) [1..4::Int] ++
     map (\_ ->  WallBlock : allBlock Empty 15) [0, 1::Int] ++
     map (const holed) [1..4::Int]

-- | Level 7
lv7 :: [[Block]]
lv7 = [allBlock WallBlock 16] ++
        map (const borders) [1..4::Int] ++
        map (\_ ->  WallBlock : allBlock Empty 15) [0, 1::Int] ++
        map (const holed) [1..4::Int]

-- | Map for level 4
lv4 :: [[Block]]
lv4 =
    [allBlock WallBlock 16] ++
    map (const borders) [1..6::Int] ++
    map (\_ -> allBlock WallBlock 16) [0, 1::Int]