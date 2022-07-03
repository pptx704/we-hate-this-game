module Level4 where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Assets
import Screens
import WeHateThisGame
import Interactions
import System.Random
import Data.Fixed (div')

-- you have to continue hitting the balls without 
-- letting any touch the ground for 30? seconds 

type Ball = (Float, Float)
type Counter = Int

renderBalls :: Theme -> [Ball] -> Picture
renderBalls _ []            = blank
renderBalls theme ((x, y) : ns) = ballPicture <> renderBalls theme ns
    where
        ballPicture = translate x y (color (getForegroundColor theme) ball)
        ball = translate 0 60 (circleSolid 30) 

drawLv4 :: State ([Ball], Counter) -> Picture
drawLv4 (State theme grid player (balls, _) losingState)
    = case (balls, losingState) of
    ([], True)  -> pictures [background, levelmap grid, player_, losingMessage]
    ([], False) -> pictures [background, levelmap grid, player_, winningMessage]
    (_, _)      -> pictures [background, levelmap grid, player_, floatingBalls]
    where
        fgcolor = getForegroundColor theme
        player_ = playerSprite theme player
        floatingBalls = renderBalls theme balls
        winningMessage = translate 200 (-400)
            (color fgcolor (scale 1.5 1.5 (Text "YOU WIN!")))
        losingMessage = translate 200 (-400)
            (color fgcolor (scale 1.5 1.5 (Text "YOU LOSE")))
        background = screenBackground theme
        levelmap = getLevelMap theme

ballTouchesBlock :: [Ball] -> Bool
ballTouchesBlock []               = False
ballTouchesBlock ((_, y) : balls) = (y <= -680) || ballTouchesBlock balls

-- fix randomness, use a range of x coorindates, so they don't intersect
-- | Generates random x-coordinates 
generateXCoord :: Int -> Int -> [Int]
generateXCoord time x
    | x == 0    = take 4 $ randomRs (2, 10) gen
    | otherwise = []
    where
        gen = mkStdGen $ div' (pi * fromIntegral time) 1 -- fix gen for randomness

-- | Decreases the y-coordinate of a ball 
decreaseBall :: Ball -> Ball
decreaseBall (x, y) = (x, y - 0.3)

-- | Decreases the y-coordinate of a list of balls
decreaseBallPositions :: [Ball] -> [Ball]
decreaseBallPositions = map decreaseBall

ycoord x = (fromIntegral x * 100.0, -200.0)

intersectsWithPlayer :: Player -> Ball -> Bool
intersectsWithPlayer (x, y, _, _) (x0, y0) = flag
    where
        -- ball's x-coordinate ranges from x0 to x0 + 50 
        -- player's x-coordinate ranges from x to x + 100 
        -- likewise for the y-coordinates 
        -- they intersect of both the x-coordinate and 
        -- y-coordinates intersect 
        x' = min (x + 100) (x0 + 50) - max x x0
        y' = min (y + 100) (y0 + 50) - max y y0
        flag = x' >= 1 && y' >= 1

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
updateWorld :: Float -> State ([Ball], Counter) -> State ([Ball], Counter)
updateWorld _ (State theme grid player ([], counter) losingState)
    = State theme grid player ([], counter) losingState
updateWorld t (State theme grid player (balls, counter) losingState)
    = case (ballTouchesBlock', counter >= 2900) of
    (False, True) -> State theme grid player ([], counter) False
    (True, _)     -> State theme grid player ([], counter) True
    (_, _)        -> newState -- add more balls to the screen 
    where
        ballTouchesBlock' = ballTouchesBlock balls
        remainingBalls = removeBallsPlayerTouches player balls
        updatedBalls = decreaseBallPositions remainingBalls
        moreBalls = map ycoord (generateXCoord counter (counter `mod` 300)) -- all the new balls 
        -- start from the same y-coordinate 
        player' = movePlayer player grid
        newState = State theme grid player' (updatedBalls ++ moreBalls, counter + 1) losingState

-- | If an specialkey (arrows for now) is pressed then generalized
-- movement function is called
handleWorld (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state

-- | For every other case, world is as is
handleWorld _ state = state

game4 :: Theme -> IO()
game4 theme = play window black 70
        (State theme lv4 (200, -600, Still, ToDown 0 0) ([(200, -200)], 0) False)
        (drawWorld drawLv4) handleWorld updateWorld