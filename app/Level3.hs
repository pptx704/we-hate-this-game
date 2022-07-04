module Level3 where

import Graphics.Gloss
import Interactions
import Screens
import Assets
import WeHateThisGame
import Graphics.Gloss.Interface.IO.Game

-- | Game level drawing function
drawLv3 :: State -> Picture
drawLv3 (State theme grid player (Lv3 ((x, y), caged)) _ _) =
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
drawLv3 _ = blank -- blank is returned if wrong state is put here

-- | If theme is changed then handle level specific mechanism
-- otherwise continues
handleWorld3 :: Event -> State -> State
handleWorld3
    (EventKey (SpecialKey KeySpace) Down (Modifiers Down _ _) _)
    (State theme grid player (Lv3 (coord, True)) w gs)
    = State theme' grid player (Lv3 (coord, newState)) w gs
    where
        theme' = changeTheme theme
        newState = theme /= DarkTheme
handleWorld3 (EventKey (SpecialKey k) pos sp _) state
    = applyMovement k pos sp state
handleWorld3 _ state = state

-- | Updates the world
-- Change balloon position, applies level specific collision
-- then applies general updateState
updateWorld3 :: Float -> State -> State
updateWorld3 _ state = newState
    where
        newState = objectGravity $ updateStates $ balloonUp state
        objectGravity st@(State t g (px, py, m, _) (Lv3 ((bx, by), c)) w gs) = 
            if px >= bx - 250 && px <= bx + 250
                && py >= by - 305 && py <= by - 285
                then State t g (px, py, m, ToDown 0 0.1) (Lv3 ((bx, by), c)) w gs
                else st
        objectGravity st = st -- Wrong state will not add balloons here
        balloonUp (State t g p (Lv3 ((bx, by), False)) w gs) =
            State t g p (Lv3 (ballonPos'(bx, by) g, False)) w gs
        balloonUp s = s
        ballonPos' (bx, by) grid = case cellCoordToType (bx, by+49.8) grid of
            Empty -> (bx, by + 1.8)
            _ -> (bx, by)