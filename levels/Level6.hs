module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

window :: Display
window = InWindow "Random window" (1600, 900) (100, 100)

data Theme = DarkTheme | LightTheme

getBackgroundColor :: Theme -> Color 
getBackgroundColor DarkTheme  = black
getBackgroundColor LightTheme = white 

getForegroundColor :: Theme -> Color 
getForegroundColor DarkTheme  = white 
getForegroundColor LightTheme = black

-- Player is a static image for now. 
-- Maybe it will animate in the final submission
playerSprite :: Theme -> Picture
playerSprite theme = pictures [head, body, hands, legs]
    where
        fgcolor = getForegroundColor theme
        head = translate 0 120 (color fgcolor (circleSolid 17.5))
        body = translate 0 60 (color fgcolor (rectangleSolid 8 85))
        hands = translate 0 60 (leftHand <> rightHand)
        legs = translate 0 (-15) (leftLeg <> rightLeg)
        hand' = color fgcolor (rectangleSolid 5 80)
        leftHand = translate (-10) 0 (rotate 15 hand')
        rightHand = translate 10 0 (rotate (-15) hand')
        leg' = color fgcolor (rectangleSolid 5 70)
        leftLeg = translate (-5) 0 (rotate 5 leg')
        rightLeg = translate 5 0 (rotate (-5) leg')

jumpingBlock :: Theme -> Picture
jumpingBlock theme = pictures [
        color (getBackgroundColor theme) (rectangleSolid 100 100),
        color (getForegroundColor theme) (rectangleSolid 98 98)
    ]

wallBlock :: Theme -> Picture
wallBlock theme = pictures [jumpingBlock theme, texture]
    where
        line= color (getBackgroundColor theme) (rectangleSolid 20 5)
        threeLine = pictures [
            line,
            translate 30 0 line,
            translate (-30) 0 line
            ]
        twoLine = pictures [
            translate 15 0 line,
            translate (-15) 0 line
            ]
        texture = pictures [
            threeLine, 
            translate 0 30 twoLine, 
            translate 0 (-30) twoLine
            ]

-- make the theme global later, somehow?

type Stone = (Int, Int, Char)
data State = State [Stone] Bool -- list of rolling stones, 
-- and a bool (False - if the game has ended)

renderStone :: Theme -> Stone -> Picture 
renderStone theme (x, y, char) = stonePicture <> characterPicture 
    where 
        fgcolor = getForegroundColor theme 
        x' = fromIntegral x
        y' = fromIntegral y
        stonePicture = translate (x' + 25.0) (y' + 20.0) 
                (color fgcolor (thickCircle 50 15))
        characterPicture = translate x' y' 
                (color fgcolor (scale 0.5 0.5 (Text [char])))

-- generates the state from a given string 
generateWorld :: String -> Int -> [Stone] 
generateWorld []       dx = []
generateWorld (n : ns) dx = (dx, 0, n) : (generateWorld ns (dx + 150))   

renderStones :: [Stone] -> Picture 
renderStones []       = blank
renderStones (n : ns) = pictures [renderStone DarkTheme n, 
                                    renderStones(ns)]

drawWorld :: State -> Picture 
drawWorld (State [] True) = pictures [player, losingMessage]
    where 
        fgcolor = getForegroundColor DarkTheme 
        player = translate (-500) 0 (playerSprite DarkTheme)
        losingMessage = translate (-390) 10
            (color fgcolor (scale 1.5 1.5 (Text ("YOU LOSE"))))
drawWorld (State [] _) = pictures [player, winningMessage]
    where 
        fgcolor = getForegroundColor DarkTheme 
        player = translate (-500) 0 (playerSprite DarkTheme)
        winningMessage = translate (-390) 10
            (color fgcolor (scale 1.5 1.5 (Text ("YOU WIN!"))))
drawWorld (State stones _) = pictures [player, rollingStones]
    where 
        player = translate (-500) (0) (playerSprite DarkTheme)
        rollingStones = renderStones stones

checkMatch :: Char -> Char -> Bool 
checkMatch x y 
    | x == y    = True 
    | otherwise = False

handleWorld :: Event -> State -> State 
handleWorld (EventKey (Char char) _ _ _) 
    (State ((x, y, firstCharacter) : remStones) losingState) = 
    newState 
    where
        matches = (checkMatch char firstCharacter)
        newState = case matches of 
                       True  -> State (remStones) losingState
                       False -> State ((x, y, firstCharacter) : remStones) 
                                   losingState

handleWorld _ state = state

decreaseStone :: Stone -> Stone 
decreaseStone (x, y, char) = (x - 100, y, char)

touchesPlayer :: [Stone] -> Bool 
touchesPlayer ((x, y, firstCharacter) : remStones) = (x <= -490)
touchesPlayer _ = True

updateWorld :: Float -> State -> State 
updateWorld step (State [] losingState) = State [] losingState
updateWorld step (State stones losingState) = newState 
    where 
        lostGame = touchesPlayer stones
        newStonesLoc = (map (decreaseStone) stones)
        newState = case lostGame of 
                       True  -> (State [] True)
                       False -> (State newStonesLoc losingState)

game :: IO () 
game = play window (getBackgroundColor DarkTheme) 1 
          (State ((generateWorld ("youhavetotypeme") 0)) False)
       drawWorld handleWorld updateWorld
-- you have to render the stones from a string 
-- the world is a string 

main :: IO ()
main = game