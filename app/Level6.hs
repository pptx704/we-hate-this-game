{-# LANGUAGE NumDecimals #-}
module Level6 where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Assets
import Screens
import WeHateThisGame
import Interactions
import System.Random
import Data.Fixed (div')

-- generates the state from a given string 
generateWorld :: String -> Float -> [Stone]
generateWorld []       _ = []
generateWorld (n : ns) dx = (dx, 0, 0, n) : generateWorld ns (dx + 150)

-- | Generates a list of chars for the stones
generateString :: [Char]
generateString = take 10 $ 
    randomRs ('a', 'z') gen
    where 
        gen = mkStdGen $ div' (pi * 40000) 1 -- fix gen for randomness

-- | (x, y, rot, char)
type Stone = (Float, Float, Float, Char)

-- | renders a Stone with a given theme (Dark | Light)
renderStone :: Theme -> Stone -> Picture
renderStone theme (x, y, rot, char) 
    = translate x y $ rotate rot (stonePicture <> characterPicture)
    where
        fgcolor = getForegroundColor theme
        stonePicture = 
             rollingStone theme
        characterPicture = translate 0 (-10) $
            scale 0.3 0.3 (color fgcolor (Text [char]))

-- | renders a list of stones 
renderStones :: Theme -> [Stone] -> Picture
renderStones _ []       = blank
renderStones theme (n : ns) 
    = renderStone theme n <> renderStones theme ns

-- | Gets the world generated on (0,0) and places it on top corner
drawWorld :: State [Stone] -> Picture
drawWorld a = translate (-750) 400 $ drawWorld' a


-- | OG world drawing function
-- if there is no stone then there is a result. Else game goes on
drawWorld' :: State [Stone] -> Picture
drawWorld' (State theme grid player stones losingState) 
    = case (stones, losingState) of
    ([], True) -> pictures [getBackground theme grid, player_, losingMessage]
    ([], False) -> pictures [getBackground theme grid, player_, winningMessage]
    (_, _) -> pictures [getBackground theme grid, player_, rollingStones]
    where
        fgcolor = getForegroundColor theme
        player_ = playerSprite theme player
        winningMessage = translate 200 (-400)
            (color fgcolor (scale 1.5 1.5 (Text "YOU WIN!")))
        losingMessage = translate 200 (-400)
            (color fgcolor (scale 1.5 1.5 (Text "YOU LOSE")))
        rollingStones = translate 0 (-600) (renderStones theme stones)


-- | Handles events
handleWorld :: Event -> State [Stone] -> State [Stone]
-- | If a character key is pressed, then stones are updated
handleWorld (EventKey (Char char) Down _ _)
    (State theme grid player 
        ((x, y, rot, firstCharacter) : remStones) 
        losingState) = newState
    where
        matches = char == firstCharacter
        newState = if matches then
            State theme grid player remStones losingState else
            State theme grid player 
                ((x, y, rot, firstCharacter) : remStones) losingState

-- | If an specialkey (arrows for now) is pressed then generalized
-- movement function is called
handleWorld (EventKey (SpecialKey k) Down _ _) state
    = applyMovement k state

-- | For every other case, world is as is
handleWorld _ state = state

-- | decreaseStone rolls the stones to the left 
decreaseStone :: Float -> Stone -> Stone
decreaseStone t (x, y, rot, char) = (x - 50*t, y, rot - t * 45, char)

-- | checks if the stone touches the player sprite 
touchesPlayer :: [Stone] -> Player -> Bool
touchesPlayer ((s, _, _,  _) : _) (x, _) = s-25 <= x+25
touchesPlayer _ _ = True

-- | Update the worlds based on time passed
updateWorld :: Float -> State [Stone] -> State [Stone]
updateWorld _ (State theme grid player [] losingState) 
    = State theme grid player [] losingState
updateWorld t (State theme grid player stones losingState) = newState
    where
        lostGame = touchesPlayer stones player
        newStonesLoc = map (decreaseStone t) stones
        newState = if lostGame then
            State theme grid player [] True else
            State theme grid player newStonesLoc losingState



-- make the theme global later, somehow?
-- data State [Stone] = State Theme [[Block]] Player [Stone] Bool -- list of rolling stones, 
-- and a bool (True - if the user has lost)
game6 :: Theme -> IO ()
game6 theme = play window (getBackgroundColor theme) 120
        (State theme lv6 (200, -600) (generateWorld generateString 700) False)
        drawWorld handleWorld updateWorld

-- game = play Display Color Int world (world -> Picture) (Event -> world -> world) (Float -> world -> world)
-- you have to render the stones from a string 
-- the world is a string 