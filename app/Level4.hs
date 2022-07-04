module Level4 where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Assets
import Screens
import WeHateThisGame
import Interactions
import System.Random

-- | Data type to store ball position
type Ball = (Float, Float)

-- | Renders a list of ball on screen
renderBalls :: Theme -> [Ball] -> Picture
renderBalls _ []            = blank
renderBalls theme ((x, y) : ns) = ballPicture <> renderBalls theme ns
    where
        ballPicture = translate x y (color (getForegroundColor theme) ball)
        ball = translate 0 60 (circleSolid 30)

-- | Level specific items are balls
drawLv4 :: State ([Ball], Int) -> Picture
drawLv4 (State theme _ _ (balls, _) _ _)
    = case balls of
    [] -> blank
    _ -> renderBalls theme balls

-- | Checks if any ball touches the ground
ballTouchesBlock :: [Ball] -> Bool
ballTouchesBlock []               = False
ballTouchesBlock ((_, y) : balls) = (y <= -680) || ballTouchesBlock balls

-- fix randomness, use a range of x coorindates, so they don't intersect
-- | Generates random x-coordinates 
generateXCoord :: Int -> Int -> [Int]
generateXCoord time x
    | x == 0    = take 4 rs
    | otherwise = []
    where
        rs = randomRs (2, 10) $ mkStdGen time

-- | Decreases the y-coordinate of a ball 
decreaseBall :: Ball -> Ball
decreaseBall (x, y) = (x, y - 0.3)

-- | Decreases the y-coordinate of a list of balls
decreaseBallPositions :: [Ball] -> [Ball]
decreaseBallPositions = map decreaseBall

-- | Takes a pair of line and finds out if they overlap
linesCollide :: Ord a => (a, a) -> (a, a) -> Bool
linesCollide (s1, e1) (s2, e2) = 
    min e1 e2 > max s1 s2

-- | Considers both player and ball as rectangle and looks for
-- overlapping area
intersectsWithPlayer :: Player -> Ball -> Bool
intersectsWithPlayer (bx, by, _, _) (px, py) = x' && y'
    where
        x' = linesCollide (bx - 25, bx + 25) (px - 30, px + 30)
        y' = linesCollide (by - 30, by + 30) (py - 50, py + 150)

-- | Removes a ball if it touches player
removeTouchingBall :: Player -> Ball -> [Ball]
removeTouchingBall player ball
    = [ball | not intersects]
    where
        intersects = intersectsWithPlayer player ball

-- | Removes all balls that touches the player
removeBallsPlayerTouches :: Player -> [Ball] -> [Ball]
removeBallsPlayerTouches _ []       = []
removeBallsPlayerTouches player (n : ns) = removeTouchingBall player n
    ++ removeBallsPlayerTouches player ns

-- | Update the worlds based on time passed
-- actually not from time passed but fps
updateWorld :: Float -> State ([Ball], Int) -> State ([Ball], Int)
updateWorld _ state@(State theme grid player (balls, counter) winningState gameState)
    = case (ballTouchesBlock', counter >= 2900) of
    (False, True) -> won
    (True, _)     -> lost
    (_, _)        -> newState
    where
        newState = case gameState of
            Paused -> state
            _ -> updateStates newState'
        ballTouchesBlock' = ballTouchesBlock balls
        remainingBalls = removeBallsPlayerTouches player balls
        updatedBalls = decreaseBallPositions remainingBalls
        ycoord x = (fromIntegral x * 100.0, -200.0)
        moreBalls = map ycoord (generateXCoord counter (counter `mod` 300)) 
        newState' = State theme grid player 
            (updatedBalls ++ moreBalls, counter + 1) winningState gameState
        won = updateStates $ State theme grid player ([], counter) True gameState
        lost = updateStates $ State theme grid player ([], counter) True gameState

-- | Handler is same as the general one
handleWorld :: Event -> State ([Ball], Int) -> State ([Ball], Int)
handleWorld (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state
handleWorld _ state = state

-- | Game function
game4 :: Theme -> IO()
game4 theme = do
    gen <- newStdGen
    play window black 70
        (State theme lv4 (200, -600, Still, ToDown 0 0.1)
            (map (\x -> (x * 100, -200)) $ take 3 $ randomRs (2, 10) gen, 0) False Resumed)
        (drawWorld drawLv4) handleWorld updateWorld