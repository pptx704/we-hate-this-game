module Level6 where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Assets
import Screens
import WeHateThisGame
import Interactions (applyMovement)

-- generates the state from a given string 
generateWorld :: String -> Int -> [Stone]
generateWorld []       _ = []
generateWorld (n : ns) dx = (dx, 0, n) : generateWorld ns (dx + 150)

type Stone = (Int, Int, Char)

-- renders a Stone with a given theme (Dark | Light)
renderStone :: Theme -> Stone -> Picture
renderStone theme (x, y, char) = stonePicture <> characterPicture
    where
        fgcolor = getForegroundColor theme
        x' = fromIntegral x
        y' = fromIntegral y
        stonePicture = translate (x' + 25.0) (y' + 20.0)
                (rollingStone theme)
        characterPicture = translate x' y'
                (color fgcolor (scale 0.5 0.5 (Text [char])))

-- renders a list of stones 
renderStones :: Theme -> [Stone] -> Picture
renderStones _ []       = blank
renderStones theme (n : ns) 
    = renderStone theme n <> renderStones theme ns

drawWorld :: State [Stone] -> Picture
drawWorld (State theme grid player stones winstate) = case (stones, winstate) of
    ([], True) -> pictures [getBackground theme grid, player_, losingMessage]
    ([], False) -> pictures [getBackground theme grid, player_, winningMessage]
    (_, _) -> pictures [getBackground theme grid, player_, rollingStones]
    where
        fgcolor = getForegroundColor theme
        player_ = playerSprite theme player
        winningMessage = translate (-390) 10
            (color fgcolor (scale 1.5 1.5 (Text "YOU WIN!")))
        losingMessage = translate (-390) 10
            (color fgcolor (scale 1.5 1.5 (Text "YOU LOSE")))
        rollingStones = translate 0 (-200) (renderStones theme stones)

checkMatch :: Char -> Char -> Bool
checkMatch x y
    | x == y    = True
    | otherwise = False

handleWorld :: Event -> State [Stone] -> State [Stone]
handleWorld (EventKey (Char char) _ _ _)
    (State theme grid player ((x, y, firstCharacter) : remStones) losingState) =
    newState
    where
        matches = checkMatch char firstCharacter
        newState = if matches then
            State theme grid player remStones losingState else
            State theme grid player ((x, y, firstCharacter) : remStones) losingState

handleWorld (EventKey (SpecialKey k) _ _ _) state
    = applyMovement k state

handleWorld _ state = state

-- decreaseStone rolls the stones to the left 
decreaseStone :: Stone -> Stone
decreaseStone (x, y, char) = (x - 100, y, char)

-- checks if the stone touches the player sprite 
touchesPlayer :: [Stone] -> Player -> Bool
touchesPlayer ((s, _, _) : _) (x, _) = fromIntegral s <= x
touchesPlayer _ _ = True

updateWorld :: Float -> State [Stone] -> State [Stone]
updateWorld _ (State theme grid player [] losingState) 
    = State theme grid player [] losingState
updateWorld _ (State theme grid player stones losingState) = newState
    where
        lostGame = touchesPlayer stones player
        newStonesLoc = map decreaseStone stones
        newState = if lostGame then
            State theme grid player [] True else
            State theme grid player newStonesLoc losingState

game :: Theme -> IO ()

-- make the theme global later, somehow?
-- data State [Stone] = State Theme [[Block]] Player [Stone] Bool -- list of rolling stones, 
-- and a bool (True - if the user has lost)
game theme = play window (getBackgroundColor theme) 1
        (State theme lv6 (-500, -200) (generateWorld "youhavetotypeme" 0) False)
        drawWorld handleWorld updateWorld

-- game = play Display Color Int world (world -> Picture) (Event -> world -> world) (Float -> world -> world)
-- you have to render the stones from a string 
-- the world is a string 