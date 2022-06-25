{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Level6 where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Assets
import Screens
import WeHateThisGame

-- make the theme global later, somehow?
data State = State Theme [Stone] Bool -- list of rolling stones, 
-- and a bool (True - if the user has lost)

-- generates the state from a given string 
generateWorld :: String -> Int -> [Stone]
generateWorld []       dx = []
generateWorld (n : ns) dx = (dx, 0, n) : generateWorld ns (dx + 150)


drawWorld :: State -> Picture
drawWorld (State theme [] True) = pictures [getBackground theme lv6, player, losingMessage]
    where
        fgcolor = getForegroundColor theme
        player = translate (-500) (-200) (playerSprite theme)
        losingMessage = translate (-390) 10
            (color fgcolor (scale 1.5 1.5 (Text "YOU LOSE")))
drawWorld (State theme [] _) = pictures [getBackground theme lv6, player, winningMessage]
    where
        fgcolor = getForegroundColor theme
        player = translate (-500) (-200) (playerSprite theme)
        winningMessage = translate (-390) 10
            (color fgcolor (scale 1.5 1.5 (Text "YOU WIN!")))
drawWorld (State theme stones _) = pictures [getBackground theme lv6, player, rollingStones]
    where
        player = translate (-500) (-200) (playerSprite theme)
        rollingStones = translate 0 (-200) (renderStones theme stones)

checkMatch :: Char -> Char -> Bool
checkMatch x y
    | x == y    = True
    | otherwise = False

handleWorld :: Event -> State -> State
handleWorld (EventKey (Char char) _ _ _)
    (State theme ((x, y, firstCharacter) : remStones) losingState) =
    newState
    where
        matches = checkMatch char firstCharacter
        newState = case matches of
                       True  -> State theme remStones losingState
                       False -> State theme ((x, y, firstCharacter) : remStones)
                                   losingState

handleWorld _ state = state

-- decreaseStone rolls the stones to the left 
decreaseStone :: Stone -> Stone
decreaseStone (x, y, char) = (x - 100, y, char)

-- checks if the stone touches the player sprite 
touchesPlayer :: [Stone] -> Bool
touchesPlayer ((x, y, firstCharacter) : remStones) = x <= -490
touchesPlayer _ = True

updateWorld :: Float -> State -> State
updateWorld step (State theme [] losingState) = State theme [] losingState
updateWorld step (State theme stones losingState) = newState
    where
        lostGame = touchesPlayer stones
        newStonesLoc = map decreaseStone stones
        newState = case lostGame of
                       True  -> State theme [] True
                       False -> State theme newStonesLoc losingState

game :: Theme -> IO ()
game theme = play window (getBackgroundColor theme) 1
          (State theme (generateWorld "youhavetotypeme" 0) False)
       drawWorld handleWorld updateWorld

-- game = play Display Color Int world (world -> Picture) (Event -> world -> world) (Float -> world -> world)
-- you have to render the stones from a string 
-- the world is a string 