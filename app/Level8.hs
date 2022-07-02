module Level8 where
import Graphics.Gloss
import Interactions
import Screens
import Assets
import WeHateThisGame
import System.Random
import Graphics.Gloss.Interface.IO.Game
import Data.List (sort, group)

-- | From the gamestate, determines Bulls and Cows
-- Cows are intentionally distracting. It doesn't negate bulls
-- to increase confusion
-- >>> getBulls ([1,2,3,4], [1,1,1,1])
-- [1,4]
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


-- | Generates 4 integers randomly that is the game solution
generateWorld :: StdGen -> [Int]
generateWorld gen = take 4 $ randomRs (0, 9) gen

-- | Game level drawing function
drawLv8 :: State ([Int], [Int]) -> Picture
drawLv8 (State theme _ player (solution, usr) winningstate) =
    if winningstate then
        pictures [background, levelmap grid', player_]
    else pictures [background, levelmap grid, player_]
    where
        background = screenBackground theme
        player_ = playerSprite theme player
        levelmap = getLevelMap theme
        grid =  lv8 (usr, bulls)
        grid' = if winningstate then changeCell (15,5) (const Empty) grid''
            else grid
        grid'' = changeCell (15,6) (const Portal) grid
        bulls = getBulls (solution, usr)


-- | If the mouse button is pressed, then the mouse coordinate
-- is used to change game state
handleWorld :: Event -> State ([Int], [Int]) -> State ([Int], [Int])
handleWorld (EventKey (MouseButton LeftButton) Down _ coord)
    (State theme grid player (solution, usr) _) =
        State theme grid player state' winningState'
        where
            cell = mouseToCell coord
            state' = (solution, changeUsr (toBeChanged cell) usr)
            toBeChanged (x, 5) = x - 6
            toBeChanged _ = -1
            changeUsr 0 (s:ss) = (s+1) `mod` 10 :ss
            changeUsr _ [] = []
            changeUsr n (s:ss) = s: changeUsr (n-1) ss
            winningState' = case getBulls state' of
                [4, 4] -> True
                _ -> False

-- | If an specialkey (arrows for now) is pressed then generalized
-- movement function is called
handleWorld (EventKey (SpecialKey k) pos _ _) state
    = applyMovement k pos state
    
-- | For every other case, world is as is
handleWorld _ state = state

-- Updates player movement if required
updateWorld :: p -> State a -> State a
updateWorld _ (State theme grid player gamestate winningstate) = newState
    where
        newState = State theme grid player' gamestate winningstate
        player' = movedPlayer player grid

-- | Game function
-- data State = State Theme [[Block]] Player ([Int], [Int]) Bool
game8 :: Theme -> IO()
game8 theme = do
    gen <- newStdGen
    play window black 90
        (State theme lv8' (200, -600, Still) (generateWorld gen, [0,0,0,0]) False)
        (drawWorld drawLv8) handleWorld updateWorld