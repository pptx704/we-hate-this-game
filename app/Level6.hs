module Level6 where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Assets
import WeHateThisGame
import Interactions

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
drawLv6 :: State -> Picture
drawLv6 (State theme _ _ (Lv6 stones) _ _)
    = case stones of
        [] -> blank
        _ -> rollingStones
    where
        rollingStones = translate 0 (-600) (renderStones theme stones)
drawLv6 _ = blank

-- | Handles events
-- If a character key is pressed, then stones are updated
handleWorld6 :: Event -> State -> State
handleWorld6 (EventKey (Char char) Down _ _)
    (State theme grid player
        initial@(Lv6 ((_, _, _, firstCharacter) : remStones))
        losingState gameState) = newState
    where
        matches = char == firstCharacter
        newState = if matches then
            State theme grid player (Lv6 remStones) losingState gameState else
            State theme grid player initial losingState gameState
handleWorld6 (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state
handleWorld6 _ state = state

-- | decreaseStone rolls the stones to the left 
decreaseStone :: Float -> Stone -> Stone
decreaseStone t (x, y, rot, char) = (x - 50*t, y, rot - t * 45, char)

-- | checks if the stone touches the player sprite 
touchesPlayer :: [Stone] -> Player -> Bool
touchesPlayer ((s, _, _,  _) : _) (x, _, _, _) = s-25 <= x+25
touchesPlayer _ _ = False

updateWorld6 :: Float -> State -> State
updateWorld6 t state@(State theme grid player (Lv6 stones) winningState gameState) 
    = newState
    where
        newState = case gameState of
            Paused -> state
            _ -> updateStates newState'
        lostGame = touchesPlayer stones player
        newState' = if lostGame then
            State theme grid player (Lv6 []) False Over
            else case stones of
                [] -> State theme grid player (Lv6 []) True gameState
                _ -> State theme grid player newStonesLoc winningState gameState
        newStonesLoc = Lv6 $ map (decreaseStone t) stones
updateWorld6 _ s = updateStates s