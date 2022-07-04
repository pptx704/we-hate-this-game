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
drawLv3 (State theme grid player ((x, y), caged) _ _) =
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

-- | If theme is changed then handle level specific mechanism
-- otherwise continues
handleWorld :: Event -> State Balloon -> State Balloon
handleWorld
    (EventKey (SpecialKey KeySpace) Down (Modifiers Down _ _) _)
    (State theme grid player (coord, True) w gs)
    = State theme' grid player (coord, newState) w gs
    where
        theme' = changeTheme theme
        newState = theme /= DarkTheme
handleWorld (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state
handleWorld _ state = state

-- | Updates the world
-- Change balloon position, applies level specific collision
-- then applies general updateState
updateWorld :: Float -> State Balloon -> State Balloon
updateWorld _ state = newState
    where
        newState = objectGravity $ updateStates $ balloonUp state
        objectGravity st@(State t g (px, py, m, _) ((bx, by), c) w gs) = 
            if px >= bx - 250 && px <= bx + 250
                && py >= by - 305 && py <= by - 285
                then State t g (px, py, m, ToDown 0 0.1) ((bx, by), c) w gs
                else st
        balloonUp (State t g p ((bx, by), False) w gs) =
            State t g p (ballonPos'(bx, by) g, False) w gs
        balloonUp s = s
        ballonPos' (bx, by) grid = case cellCoordToType (bx, by+49.8) grid of
            Empty -> (bx, by + 1.8)
            _ -> (bx, by)

-- | Game function
game3 :: Theme -> IO()
game3 theme = play window black 90
        (State theme lv3 (200, -600, Still, ToDown 0 1) ((750, -700), True) True Resumed)
        (drawWorld drawLv3) handleWorld updateWorld