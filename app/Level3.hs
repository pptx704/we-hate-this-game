module Level3 where

import Graphics.Gloss
import Interactions
import Screens
import Assets
import WeHateThisGame
import Graphics.Gloss.Interface.IO.Game

-- Balloon = Coordinated Caged? 
type Balloon = ((Float, Float), Bool)

-- | Game level drawing function
drawLv3 :: State Balloon -> Picture
drawLv3 (State theme grid player ((x, y), caged) _) =
    pictures [background, levelmap grid, player_, balloon']
    where
        background = screenBackground theme
        player_ = playerSprite theme player
        levelmap = getLevelMap theme
        balloon' = if caged then
            translate x y (balloon theme) <> translate x y (cage theme)
            else translate x y (balloon theme <> translate 0 (-385) jumpingBlocks)
        jumpingBlocks = jb <> translate (-200) 0 jb2 <>  translate 100 0 jb2
        jb = drawBlock theme JumpingBlock
        jb2 = jb <> translate 100 0 jb

-- | If an specialkey (arrows for now) is pressed then generalized
-- movement function is called
handleWorld :: Event -> State Balloon -> State Balloon
handleWorld
    (EventKey (SpecialKey KeySpace) Down (Modifiers Down _ _) _)
    (State theme grid player (coord, True) w)
    = State theme' grid player (coord, newState) w
    where
        theme' = changeTheme theme
        newState = theme /= DarkTheme

handleWorld (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state
-- | For every other case, world is as is
handleWorld _ state = state

updateWorld :: Float -> State Balloon -> State Balloon
updateWorld _ (State theme grid player ((x', y'), caged) winningState) = newState
    where
        newState = State theme grid player' (coords, caged) winningState
        player'' = movePlayer player grid
        player' = objectGravity player'' (x', y')
        coords = if caged then (x', y') else newPos (x', y') (x', y'+4)
        newPos (a1, a2) (b1, b2) = case cellCoordToType (b1, b2+45)  grid of
            Empty -> (b1, b2)
            _ -> (a1, a2)

        -- | Checks if sprite collides with additional block or not
        objectGravity (px, py, m, j) (bx, by) = 
            if px >= bx - 250 && px <= bx + 250
                -- Not possible to find exact value of jumping
                -- that's why a range is checked to find the landing position
                && py >= by - 305 && py <= by - 285
            then (px, py, m, ToDown 0 0.1)
            else (px, py, m, j) 

-- | Game function
-- data State = State Theme [[Block]] Player Balloon Bool
game3 :: Theme -> IO()
game3 theme = play window black 90
        (State theme lv3 (200, -600, Still, ToDown 0 1) ((750, -700), True) True)
        (drawWorld drawLv3) handleWorld updateWorld

