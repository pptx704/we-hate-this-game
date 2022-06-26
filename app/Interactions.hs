module Interactions where
import Graphics.Gloss.Interface.Pure.Game
import WeHateThisGame

applyMovement :: SpecialKey -> State a -> State a
applyMovement k (State theme grid (x, y) stones losingState)
    = case k of
        KeyRight -> State theme grid (x+30, y) stones losingState
        KeyLeft -> State theme grid (x-30, y) stones losingState
        _ -> State theme grid (x, y) stones losingState

-- changeCellAtCol :: Int -> (Block -> Block) -> [Block] -> [Block]
-- changeCellAtCol _ _ [] = []
-- changeCellAtCol 0 f (x:xs) = f x:xs
-- changeCellAtCol n f (x:xs) = x: changeCellAtCol (n-1) f xs

-- changeCell :: (Int, Int) -> (Block -> Block) -> [[Block]] -> [[Block]]
-- changeCell _ _ [] = [[]]
-- changeCell (x, 0) func (g:gs) = changeCellAtCol x func g:gs
-- changeCell (x, y) func (g:gs) = g : changeCell (x, y-1) func gs 

-- getCellAtCol :: Int -> [Block] -> Block
-- getCellAtCol 0 (x:_) = x
-- getCellAtCol n (_:xs) = getCellAtCol (n-1) xs
-- getCellAtCol _ [] = Empty

-- getCellType :: (Int, Int) -> [[Block]] -> Block
-- getCellType (x, 0) (g:_) = getCellAtCol x g
-- getCellType (x, y) (_:gs) = getCellType (x, y - 1) gs
-- getCellType _ [] = Empty


-- getCellPos :: Player -> (Int, Int)
-- getCellPos (x, y) = (div' (x + 800) 100, 7 - div' (y+400) 100)

-- applyMovement' :: SpecialKey -> State a -> State a
-- applyMovement' k (State theme grid (x, y) stones losingState)
--     = case k of
--         KeyRight -> State theme grid (newPos (x,y) (x+30, y)) stones losingState
--         KeyLeft -> State theme grid (newPos (x,y) (x-30, y)) stones losingState
--         KeySpace -> State theme grid' (x, y) stones losingState
--         KeyUp -> State theme grid (x, y+30) stones losingState
--         KeyDown -> State theme grid (x, y-30) stones losingState
--         _ -> State theme grid (x, y) stones losingState
--         where
--             newPos a b = case getCellType (getCellPos a) grid of
--                 Empty -> b
--                 _ -> a

--             grid' = changeCell (getCellPos (x, y)) (const JumpingBlock) grid