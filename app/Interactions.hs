module Interactions where
import Graphics.Gloss.Interface.Pure.Game
import WeHateThisGame
import Data.Fixed (div')

-- | The following two functions are used to change a block from it's index
changeCellAtCol :: Int -> (Block -> Block) -> [Block] -> [Block]
changeCellAtCol _ _ [] = []
changeCellAtCol 0 f (x:xs) = f x:xs
changeCellAtCol n f (x:xs) = x: changeCellAtCol (n-1) f xs

changeCell :: (Int, Int) -> (Block -> Block) -> [[Block]] -> [[Block]]
changeCell _ _ [] = [[]]
changeCell (x, 0) func (g:gs) = changeCellAtCol x func g:gs
changeCell (x, y) func (g:gs) = g : changeCell (x, y-1) func gs 


-- | The following two functions are used to find the type of a block
getCellAtCol :: Int -> [Block] -> Block
getCellAtCol 0 (x:_) = x
getCellAtCol n (_:xs) = getCellAtCol (n-1) xs
getCellAtCol _ [] = Empty

getCellType :: (Int, Int) -> [[Block]] -> Block
getCellType (x, 0) (g:_) = getCellAtCol x g
getCellType (x, y) (_:gs) = getCellType (x, y - 1) gs
getCellType _ [] = Empty

-- | changeCell and getCell can be merged or something??

-- | Converts a coordinate to the cell inside the grid syste
-- >>> getCellPos (200, -600)
-- NOW (2,6)
getCellPos :: Player -> (Int, Int)
getCellPos (x, y) = (div' x 100, abs $ div' y 100)


-- | Generalized movement function. Checks if a the new grid position of the
-- player is a block or empty. If empty then player moves, else not.
applyMovement :: SpecialKey -> State a -> State a
applyMovement k (State theme grid (x, y) stones losingState)
    = case k of
        KeyRight -> State theme grid (newPos (x,y) (x+30, y)) stones losingState
        KeyLeft -> State theme grid (newPos (x,y) (x-30, y)) stones losingState
        -- the following 3 are for testing purpose. Will be removed
        -- KeyUp -> State theme grid (newPos (x,y) (x, y+30)) stones losingState
        -- KeyDown -> State theme grid (newPos (x,y) (x, y-30)) stones losingState
        KeySpace -> State theme (changeCell (getCellPos (x, y)) (const $ NumberedBlock 5) grid) (x, y) stones losingState
        _ -> State theme grid (x, y) stones losingState
        where
            newPos a b = case getCellType (getCellPos b) grid of
                Empty -> b
                _ -> a