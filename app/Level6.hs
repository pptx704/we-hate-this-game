module Level6 where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Assets
import Screens
import WeHateThisGame
import Interactions
import System.Random

-- generates the state from a given string 
generateWorld :: String -> Float -> [Stone]
generateWorld []       _ = []
generateWorld (n : ns) dx = (dx, 0, 0, n) : generateWorld ns (dx + 150)

-- | Generates a list of chars for the stones
generateString :: StdGen -> [Char]
generateString gen = take 15 $
    randomRs ('a', 'z') gen

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


-- | OG world drawing function
-- if there is no stone then there is a result. Else game goes on
drawLv6 :: State [Stone] -> Picture
drawLv6 (State theme _ _ stones _ _)
    = case stones of
        [] -> blank
        _ -> rollingStones
    where
        rollingStones = translate 0 (-600) (renderStones theme stones)

-- | Handles events
-- If a character key is pressed, then stones are updated
handleWorld :: Event -> State [Stone] -> State [Stone]
handleWorld (EventKey (Char char) Down _ _)
    (State theme grid player
        initial@((_, _, _, firstCharacter) : remStones)
        losingState gameState) = newState
    where
        matches = char == firstCharacter
        newState = if matches then
            State theme grid player remStones losingState gameState else
            State theme grid player initial losingState gameState
handleWorld (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state
handleWorld _ state = state

-- | decreaseStone rolls the stones to the left 
decreaseStone :: Float -> Stone -> Stone
decreaseStone t (x, y, rot, char) = (x - 50*t, y, rot - t * 45, char)

-- | checks if the stone touches the player sprite 
touchesPlayer :: [Stone] -> Player -> Bool
touchesPlayer ((s, _, _,  _) : _) (x, _, _, _) = s-25 <= x+25
touchesPlayer _ _ = False

updateWorld :: Float -> State [Stone] -> State [Stone]
updateWorld t state@(State theme grid player stones winningState gameState) 
    = newState
    where
        newState = case gameState of
            Paused -> state
            _ -> updateStates newState'
        lostGame = touchesPlayer stones player
        newState' = if lostGame then
            State theme grid player [] False Over
            else case stones of
                [] -> State theme grid player [] True gameState
                _ -> State theme grid player newStonesLoc winningState gameState
        newStonesLoc = map (decreaseStone t) stones


-- game function
game6 :: Theme -> IO ()
game6 theme = do
    gen <- newStdGen
    play window black 90
        (State theme lv6 (200, -600, Still, ToDown 0 0) (generateWorld (generateString gen) 700) False
            Resumed)
        (drawWorld drawLv6) handleWorld updateWorld