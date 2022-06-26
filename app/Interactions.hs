module Interactions where
import Graphics.Gloss.Interface.Pure.Game
import WeHateThisGame

applyMovement :: SpecialKey -> State a -> State a
applyMovement k (State theme grid (x, y) stones losingState)
    = case k of
        KeyRight -> State theme grid (x+30, y) stones losingState
        KeyLeft -> State theme grid (x-30, y) stones losingState
        _ -> State theme grid (x, y) stones losingState