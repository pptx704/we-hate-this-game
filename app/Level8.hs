module Level8 where

import WeHateThisGame
import Assets
import Graphics.Gloss

data State = State Theme [Int] [Block]

isStateSolved :: State -> Bool
isStateSolved (State _ [] []) = True
isStateSolved (State a (sol: sols) (NumberedBlock st: sts))
    = sol == st && isStateSolved (State a sols sts)
isStateSolved State {} = False

getRandomNumber :: Int
getRandomNumber = 230

getDigitList :: [Int]
getDigitList = dig getRandomNumber
    where
        dig 0 = []
        dig a = a `mod` 10: dig (a `div` 10)

drawNumberedStates :: Theme -> [Block] -> Picture
drawNumberedStates _ [] = blank
drawNumberedStates theme (NumberedBlock a:xs) = numberedBlock theme a
        <> translate 200 0 (drawNumberedStates theme xs)
drawNumberedStates _ _ = blank

changeNumberedState :: Block -> Block
changeNumberedState (NumberedBlock a) = NumberedBlock $ (a + 1) `mod` 10
changeNumberedState a = a