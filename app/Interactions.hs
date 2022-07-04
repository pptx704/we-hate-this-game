module Interactions where
import Graphics.Gloss.Interface.Pure.Game
import WeHateThisGame
import Data.Fixed (div')
import Screens
import System.Random

-- | Changes the theme
changeTheme :: Theme -> Theme
changeTheme LightTheme = DarkTheme
changeTheme DarkTheme = LightTheme

-- | The following two functions are used to change a block from it's index
changeCellAtCol :: Int -> Block -> [Block] -> [Block]
changeCellAtCol _ _ [] = []
changeCellAtCol 0 b (_:xs) = b:xs
changeCellAtCol n b (x:xs) = x: changeCellAtCol (n-1) b xs

changeCell :: (Int, Int) -> Block -> [[Block]] -> [[Block]]
changeCell _ _ [] = [[]]
changeCell (x, 0) b (g:gs) = changeCellAtCol x b g:gs
changeCell (x, y) b (g:gs) = g : changeCell (x, y-1) b gs


-- | The following two functions are used to find the type of a block
getCellAtCol :: Int -> [Block] -> Block
getCellAtCol 0 (x:_) = x
getCellAtCol n (_:xs) = getCellAtCol (n-1) xs
getCellAtCol _ [] = Empty

getCellType :: (Int, Int) -> [[Block]] -> Block
getCellType (x, 0) (g:_) = getCellAtCol x g
getCellType (x, y) (_:gs) = getCellType (x, y - 1) gs
getCellType _ [] = Empty

-- | Converts a coordinate to the cell inside the grid syste
-- >>> getCellPos (250, -650)
-- NOW (3,6)
getCellPos :: (Float, Float) -> (Int, Int)
getCellPos (x, y) = (div' (x+50) 100, abs $ div' (y+50) 100)

-- | Gets the type of a cell from the coordinates
cellCoordToType :: (Float, Float) -> [[Block]] -> Block
cellCoordToType = getCellType . getCellPos

-- From a mouse position, determines the cell it has clicked on
mouseToCell :: (Float, Float) -> (Int, Int)
mouseToCell (x, y) = getCellPos (x+750, y-500)

-- Moves player left or right
moveToSide :: Player -> [[Block]] -> Player
moveToSide player@(x, y, m, j) grid =
    case m of
        Still -> player
        ToRight -> newPos (x, y) (x + movementCoeff, y) 30
        ToLeft -> newPos (x, y) (x - movementCoeff, y) (-30)
    where
        newPos (a1, a2) (b1, b2) c
            = case (upperSideCell b1 b2 c, lowerSideCell b1 b2 c) of
            (Empty, Empty) -> (b1, b2, m, j)
            (Portal, Empty) -> (b1, b2, m, j)
            (Empty, Portal) -> (b1, b2, m, j)
            _ -> (a1, a2, m, j)
        movementCoeff = 2.0
        upperSideCell b1 b2 c = cellCoordToType (b1+c,b2) grid
        lowerSideCell b1 b2 c = cellCoordToType (b1+c, b2+50) grid


-- | Checks if player is out of window
playerOutOfScreen :: Player -> Bool
playerOutOfScreen (x, y, _, _) = x+30 < 0 || x-30 > 1600 || y-50 > 0 || y+150 < -900

-- | Deals with jumping and gravity
applyGravity :: Player -> [[Block]] -> Player
applyGravity (x, y, m, ToDown v t) grid = player'
    where
        player' = newPos (x, y) (x, y - v')
        newPos (a1, a2) (b1, b2) = case cellCoordToType (b1, b2-50) grid of
            Empty -> (b1, b2, m, ToDown v' (t+0.1))
            _ -> (a1, a2, m, ToDown 0 0.1)
        v' = v + 0.02 * t
applyGravity (x, y, m, ToUp v t) grid = player'
    where
        player' = newPos (x, y) (x, y + v')
        newPos (a1, a2) (b1, b2) = case cellCoordToType (b1, b2+150) grid of
            Empty -> (b1, b2, m, d)
            _ -> (a1, a2, m, ToDown 0 0.1)
        d = if v' < 0 then ToDown 0 0.1 else ToUp v' (t+0.1)
        v' = v - 0.02 * t


-- | Player movement is Side moves + jump moves
movePlayer :: Player -> [[Block]] -> Player
movePlayer player grid = applyGravity (moveToSide player grid) grid


-- | Checks if user reaches the portal
reachesPortal :: Player -> Bool
reachesPortal (x, y, _, _) = x >= 1460 && x <= 1540
    && y <= -550  && y >= -650

-- | Update the game state based on player movement and inputs
updateStates :: State -> State
updateStates state@(State theme grid player stateVar winningState gameState)
    = case gameState of
        Over -> state
        Paused -> state
        Completed -> getNewLevel state
        _ -> State theme grid' player' stateVar winningState gs
    where
        gs = if playerOutOfScreen player' then Over else gs''
        gs'' = if reachesPortal player' then Completed else gameState
        player' = movePlayer player grid
        grid' = if winningState then grid'' else grid
        grid'' = changeCell (15, 5) Empty $ changeCell (15, 6) Portal grid

-- | Provides new level from the current level
getNewLevel :: State -> State
getNewLevel state@(State theme _ _ lv _ _) =
    case lv of
        Lv0 _ -> State theme lv3 (200, -600, Still, ToDown 0 1) (Lv3 ((750, -700), True)) True Resumed
        Lv3 _ -> State theme lv4 (200, -600, Still, ToDown 0 0.1)
            (Lv4 (map (\x -> (x * 100, -200)) $ take 3
            $ randomRs (2, 10) gen, 0)) False Resumed
        Lv4 _ -> State theme lv5 (200, -600, Still, ToDown 0 1) (Lv5 Empty) True Resumed
        Lv5 _ -> State theme lv6 (200, -600, Still, ToDown 0 0)
            (Lv6 (generateWorld generateString 700)) False Resumed
        Lv6 _ -> State theme lv7 (200, -600, Still, ToDown 0 1) (Lv7 0) True Resumed
        Lv7 _ -> State theme lv8' (200, -600, Still, ToDown 0 1) (Lv8 (generateWorld8, [0,0,0,0])) False Resumed
        Lv8 _ -> state
        where
            gen = (mkStdGen . round) (4000 * pi::Float)
            -- | For lv 6
            -- Generates a list of stones from a string
            generateWorld []       _ = []
            generateWorld (n : ns) dx = (dx, 0, 0, n) : generateWorld ns (dx + 150)
            -- Generates a list of chars for the stones
            generateString = take 15 $ randomRs ('a', 'z') gen
            -- | Generates 4 integers randomly that is the game solution
            generateWorld8 :: [Int]
            generateWorld8 = take 4 $ randomRs (0, 9) gen

-- | Generalized function to deal with KeyPressing.
-- Apart from movement keys, it also deals with hotkeys
-- Modifiers shift ctrl alt
applyMovement :: SpecialKey -> KeyState -> Modifiers -> State -> State
applyMovement k Down (Modifiers Down _ _) state@(State theme grid player stateVar winningState gameState)
    = case k of
        KeySpace -> State (changeTheme theme) grid player stateVar winningState gameState
        KeyUp -> State theme grid player stateVar winningState Paused
        KeyDown -> State theme grid player stateVar winningState Resumed
        _ -> state
applyMovement k pos _ state@(State theme grid player@(x, y, d, j) stateVar winningState gameState)
    = case gameState of
        Over -> state
        Paused -> state
        Completed -> getNewLevel state
        _ -> State theme grid player' stateVar winningState gameState
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