module Interactions where
import Graphics.Gloss.Interface.Pure.Game
import WeHateThisGame
import Data.Fixed (div')

-- | Changes the theme
changeTheme :: Theme -> Theme
changeTheme LightTheme = DarkTheme
changeTheme DarkTheme = LightTheme

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
-- (2,6)
getCellPos :: (Float, Float) -> (Int, Int)
getCellPos (x, y) = (div' x 100, abs $ div' y 100)


-- From a mouse position, determines the cell it has clicked on
mouseToCell :: (Float, Float) -> (Int, Int)
mouseToCell (x, y) = getCellPos (x+800, y-450)

-- 
movedPlayer :: (Float, Float, Movement) -> [[Block]] -> (Float, Float, Movement)
movedPlayer player@(x, y, m) grid =
    case m of
        Still -> player
        ToRight -> newPos (x, y) (x+movementCoeff, y)
        ToLeft -> newPos (x, y) (x-movementCoeff, y)
    where
        newPos (a1, a2) b@(b1, b2) = case getCellType (getCellPos b) grid of
            Empty -> (b1, b2, m)
            _ -> (a1, a2, m)
        movementCoeff = 2.0

-- | Generalized movement function. Checks if a the new grid position of the
-- player is a block or empty. If empty then player moves, else not.
applyMovement :: SpecialKey -> KeyState -> State a -> State a
-- Temporary applymovement for theme changing
applyMovement KeySpace Down (State theme grid (x, y, m) stateVar losingState) 
    = State (changeTheme theme) grid (x, y, m) stateVar losingState

applyMovement k pos (State theme grid (x, y, m) stateVar losingState) =
    State theme grid player' stateVar losingState
    where
        player' = case (k, pos) of
            (KeyRight, Down) -> (x, y, ToRight)
            (KeyRight, Up) -> (x, y, Still)
            (KeyLeft, Down) -> (x, y, ToLeft)
            (KeyLeft, Up) -> (x, y, Still)
            _ -> (x, y, m)
