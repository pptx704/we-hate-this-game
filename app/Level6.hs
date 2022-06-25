{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Level6 where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Assets
import Screens
import WeHateThisGame

-- make the theme global later, somehow?

type Stone = (Int, Int, Char)
data State = State [Stone] Bool -- list of rolling stones, 
-- and a bool (True - if the user has lost)

-- renders a Stone with a given theme (Dark | Light)
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
generateWorld (n : ns) dx = (dx, 0, n) : generateWorld ns (dx + 150)

-- renders a list of stones 
renderStones :: [Stone] -> Picture
renderStones []       = blank
renderStones (n : ns) = pictures [renderStone DarkTheme n,
                                    renderStones ns]

drawWorld :: State -> Picture
drawWorld (State [] True) = pictures [player, losingMessage]
    where
        fgcolor = getForegroundColor DarkTheme
        player = translate (-500) 0 (playerSprite DarkTheme)
        losingMessage = translate (-390) 10
            (color fgcolor (scale 1.5 1.5 (Text "YOU LOSE")))
drawWorld (State [] _) = pictures [player, winningMessage]
    where
        fgcolor = getForegroundColor DarkTheme
        player = translate (-500) 0 (playerSprite DarkTheme)
        winningMessage = translate (-390) 10
            (color fgcolor (scale 1.5 1.5 (Text "YOU WIN!")))
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
        matches = checkMatch char firstCharacter
        newState = case matches of
                       True  -> State remStones losingState
                       False -> State ((x, y, firstCharacter) : remStones)
                                   losingState

handleWorld _ state = state

-- decreaseStone rolls the stones to the left 
decreaseStone :: Stone -> Stone
decreaseStone (x, y, char) = (x - 100, y, char)

-- checks if the stone touches the player sprite 
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
          (State (generateWorld "youhavetotypeme" 0) False)
       drawWorld handleWorld updateWorld
-- you have to render the stones from a string 
-- the world is a string 