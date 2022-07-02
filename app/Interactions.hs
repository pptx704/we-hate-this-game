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
-- >>> getCellPos (250, -650)
-- NOW (3,6)
getCellPos :: (Float, Float) -> (Int, Int)
getCellPos (x, y) = (div' (x+50) 100, abs $ div' (y+50) 100)


-- From a mouse position, determines the cell it has clicked on
mouseToCell :: (Float, Float) -> (Int, Int)
mouseToCell (x, y) = getCellPos (x+750, y-500)

-- 
movePlayer' :: Player -> [[Block]] -> Player
movePlayer' player@(x, y, m, j) grid =
    case m of
        Still -> player
        ToRight -> newPos (x, y) (x + movementCoeff, y)
        ToLeft -> newPos (x, y) (x - movementCoeff, y)
    where
        newPos (a1, a2) b@(b1, b2) = case getCellType (getCellPos b) grid of
            Empty -> (b1, b2, m, j)
            Portal -> (b1, b2, m, j)
            _ -> (a1, a2, m, j)
        movementCoeff = 2.0

movePlayer :: Player -> [[Block]] -> Player
movePlayer player grid = applyGravity (movePlayer' player grid) grid

applyGravity :: Player -> [[Block]] -> Player
applyGravity (x, y, m, ToDown v t) grid = player'
    where
        player' = newPos (x, y) (x, y - v')
        newPos (a1, a2) (b1, b2) = case getCellType (getCellPos (b1, b2-50)) grid of
            Empty -> (b1, b2, m, ToDown v' (t+0.1))
            _ -> (a1, a2, m, ToDown 0 0.1)
        v' = v + 0.02 * t
applyGravity (x, y, m, ToUp v t) grid = player'
    where
        player' = newPos (x, y) (x, y + v')
        newPos (a1, a2) (b1, b2) = case getCellType (getCellPos (b1, b2+150)) grid of
            Empty -> (b1, b2, m, d)
            _ -> (a1, a2, m, ToDown 0 0.1)
        d = if v' < 0 then ToDown 0 0.1 else ToUp v' (t+0.1)
        v' = v - 0.02 * t

-- | Generalized movement function. Checks if a the new grid position of the
-- player is a block or empty. If empty then player moves, else not.
applyMovement :: SpecialKey -> KeyState -> Modifiers -> State a -> State a
-- Temporary applymovement for theme changing
-- Modifiers shift ctrl alt
applyMovement KeySpace Down (Modifiers Down _ _) (State theme grid player stateVar losingState)
    = State (changeTheme theme) grid player stateVar losingState

applyMovement k pos _ (State theme grid player@(x, y, d, j) stateVar losingState) =
    State theme grid player' stateVar losingState
    where
        player' = case (k, pos) of
            (KeyRight, Down) -> (x, y, ToRight, j)
            (KeyRight, Up) -> (x, y, Still, j)
            (KeyLeft, Down) -> (x, y, ToLeft, j)
            (KeyLeft, Up) -> (x, y, Still, j)
            (KeySpace, Down) -> case j of
                ToDown 0 _ -> (x, y, d, ToUp 5 0.1)
                _ -> player
            _ -> player
