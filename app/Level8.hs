module Level8 where
import Graphics.Gloss
import Interactions
import Screens
import Assets
import WeHateThisGame
import System.Random
import Graphics.Gloss.Interface.IO.Game
import Data.List

-- | From the gamestate, determines Bulls and Cows
-- Cows are intentionally distracting. It doesn't negate bulls
-- to increase confusion
-- >>> getBulls ([7,5,5,3], [7,5,5,3])
-- [4,4]
-- >>> getBulls ([1,2,3,4], [1,3,2,4])
-- [2,4]
getBulls :: ([Int], [Int]) -> [Int]
getBulls (state, usr) = [bulls, cows]
    where
        bulls' [] = 0
        bulls' ((a,b): c)
            | a == b = 1 + bulls' c
            | otherwise = bulls' c
        bulls = bulls' $ zip state usr
        cows' [] _ = 0
        cows' (a:as) c = cows'' a c + cows' as c
        cows'' _ [] = 0
        cows'' a (c:cs)
            | a == c = 1 + cows'' a cs
            | otherwise = cows'' a cs
        cows = cows' refined state
        -- | Refined removed duplicates for correct Cows count
        -- `head` is not unsafe technically as group doesn't return empty lists
        refined = map head $ group $ sort usr


-- | Game level drawing function
drawLv8 :: State -> Picture
drawLv8 (State theme grid player _ _ _) =
    pictures [background, levelmap grid, player_]
    where
        background = screenBackground theme
        player_ = playerSprite theme player
        levelmap = getLevelMap theme


-- | If the mouse button is pressed, then the mouse coordinate
-- is used to change game state
handleWorld :: Event -> State -> State
handleWorld (EventKey (MouseButton LeftButton) Down _ coord)
    state@(State theme _ player (Lv8 (solution, usr)) _ gameState) =
        case gameState of
            Resumed -> State theme grid' player (Lv8 state') winningState gameState
            _ -> state
        where
            cell = mouseToCell coord
            state' = (solution, changed)
            changed = changeUsr (toBeChanged cell) usr
            toBeChanged (x, 5) = x - 6
            toBeChanged _ = -1
            changeUsr 0 (s:ss) = (s+1) `mod` 10 :ss
            changeUsr _ [] = []
            changeUsr n (s:ss) = s: changeUsr (n-1) ss
            winningState = case getBulls state' of
                [4, 4] -> True
                _ -> False
            grid' = lv8 (changed, getBulls state')

-- | If an specialkey (arrows for now) is pressed then generalized
-- movement function is called
handleWorld (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state
    
-- | For every other case, world is as is
handleWorld _ state = state

-- Player movement is generalized
updateWorld :: Float -> State -> State
updateWorld _ = updateStates