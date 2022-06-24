module Level8 where

import WeHateThisGame
import Assets
import Graphics.Gloss

data State = State [Int] [NumberedState]

isStateSolved :: State -> Bool
isStateSolved (State [] []) = True
isStateSolved (State (sol: sols) (NumberedState st: sts))
    = sol == st && isStateSolved (State sols sts)
isStateSolved (State _ _) = False