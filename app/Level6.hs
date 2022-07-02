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
generateString gen = take 10 $ 
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
drawLv6 (State theme grid player stones losingState) 
    = case (stones, losingState) of
    ([], True) -> pictures [background, levelmap grid, player_, losingMessage]
    ([], False) -> pictures [background, levelmap grid', player_, winningMessage]
    (_, _) -> pictures [background, levelmap grid, player_, rollingStones]
    where
        fgcolor = getForegroundColor theme
        player_ = playerSprite theme player
        winningMessage = translate 250 (-400)
            (color fgcolor (scale 1.5 1.5 (Text "YOU WIN!")))
        losingMessage = translate 250 (-400)
            (color fgcolor (scale 1.5 1.5 (Text "YOU LOSE")))
        rollingStones = translate 0 (-600) (renderStones theme stones)
        grid' = changeCell (15, 6) (const Portal) grid
        background = screenBackground theme
        levelmap = getLevelMap theme

-- | Handles events
handleWorld :: Event -> State [Stone] -> State [Stone]
-- | If a character key is pressed, then stones are updated
handleWorld (EventKey (Char char) Down _ _)
    (State theme grid player 
        initial@((_, _, _, firstCharacter) : remStones) 
        losingState) = newState
    where
        matches = char == firstCharacter
        newState = if matches then
            State theme grid player remStones losingState else
            State theme grid player initial losingState


-- | If an specialkey (arrows for now) is pressed then generalized
-- movement function is called
handleWorld (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state
-- | For every other case, world is as is
handleWorld _ state = state

-- | decreaseStone rolls the stones to the left 
decreaseStone :: Float -> Stone -> Stone
decreaseStone t (x, y, rot, char) = (x - 50*t, y, rot - t * 45, char)

-- | checks if the stone touches the player sprite 
touchesPlayer :: [Stone] -> Player -> Bool
touchesPlayer ((s, _, _,  _) : _) (x, _, _, _) = s-25 <= x+25
touchesPlayer _ _ = True

-- | Update the worlds based on time passed
updateWorld :: Float -> State [Stone] -> State [Stone]
updateWorld _ currentState@(State _ _ _ [] _) 
    = currentState
updateWorld t (State theme grid player stones losingState) = newState
    where
        lostGame = touchesPlayer stones player
        newStonesLoc = map (decreaseStone t) stones
        newState = if lostGame then
            State theme grid player' [] True else
            State theme grid player' newStonesLoc losingState    
        player' = movePlayer player grid
        

-- make the theme global later, somehow?
-- data State [Stone] = State Theme [[Block]] Player [Stone] Bool
game6 :: Theme -> IO ()
game6 theme = do
    gen <- newStdGen
    play window black 90
        (State theme lv6 (200, -600, Still, ToDown 0 0) (generateWorld (generateString gen) 700) False)
        (drawWorld drawLv6) handleWorld updateWorld