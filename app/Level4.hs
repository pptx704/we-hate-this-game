module Level4 where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Assets
import Screens
import WeHateThisGame
import Interactions
import System.Random

-- you have to continue hitting the balls without 
-- letting any touch the ground for 30? seconds 

type Ball = (Float, Float)

renderBalls :: Theme -> [Ball] -> Picture
renderBalls _ []            = blank
renderBalls theme ((x, y) : ns) = ballPicture <> renderBalls theme ns
    where
        ballPicture = translate x y (color (getForegroundColor theme) ball)
        ball = translate 0 60 (circleSolid 30) 

drawLv4 :: State ([Ball], Int) -> Picture
drawLv4 (State theme grid player (balls, _) losingState _)
    = case (balls, losingState) of
    ([], True)  -> pictures [background, levelmap grid, player_]
    ([], False) -> pictures [background, levelmap grid, player_]
    (_, _)      -> pictures [background, levelmap grid, player_, floatingBalls]
    where
        player_ = playerSprite theme player
        floatingBalls = renderBalls theme balls
        background = screenBackground theme
        levelmap = getLevelMap theme

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

intersectsWithPlayer :: Player -> Ball -> Bool
intersectsWithPlayer (x, y, _, _) (x0, y0) = flag
    where
        x' = min (x + 80) (x0 + 50) - max x x0
        y' = min (y + 100) (y0 + 50) - max y y0
        flag = (x' > 0 && y' > 0)

removeTouchingBall :: Player -> Ball -> [Ball]
removeTouchingBall player ball
    = [ball | not intersects]
    where
        intersects = intersectsWithPlayer player ball

removeBallsPlayerTouches :: Player -> [Ball] -> [Ball]
removeBallsPlayerTouches _ []       = []
removeBallsPlayerTouches player (n : ns) = removeTouchingBall player n
    ++ removeBallsPlayerTouches player ns

-- | Update the worlds based on time passed
updateWorld :: Float -> State ([Ball], Int) -> State ([Ball], Int)
updateWorld _ currentState@(State _ _ _ _ _ Over)
    = currentState
updateWorld _ currentState@(State _ _ _ _ _ Paused)
    = currentState 
updateWorld t (State theme grid player (balls, counter) losingState gameState)
    = case (ballTouchesBlock', counter >= 2900) of
    (False, True) -> State theme grid player ([], counter) False Completed  
    (True, _)     -> State theme grid player ([], counter) True Over 
    (_, _)        -> newState -- add more balls to the screen 
    where
        ballTouchesBlock' = ballTouchesBlock balls
        remainingBalls = removeBallsPlayerTouches player balls
        updatedBalls = decreaseBallPositions remainingBalls
        ycoord x = (fromIntegral x * 100.0, -200.0)
        moreBalls = map ycoord (generateXCoord counter (counter `mod` 300)) -- all the new balls 
        -- start from the same y-coordinate 

        player' = movePlayer player grid
        newState = State theme grid player' (updatedBalls ++ moreBalls, counter + 1) losingState
            gameState

handleWorld (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state

-- | For every other case, world is as is
handleWorld _ state = state

game4 :: Theme -> IO()
game4 theme = do
    gen <- newStdGen
    play window black 70
        (State theme lv4 (200, -600, Still, ToDown 0 0) 
            (map (\x -> (x * 100, -200)) $ take 3 $ randomRs (2, 10) gen, 0) False Resumed)
        (drawWorld drawLv4) handleWorld updateWorld