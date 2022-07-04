module Main where
import WeHateThisGame
import Level0 (game0)
import Level3 (game3)
import Level4 (game4)
import Level5 (game5)
import Level6 (game6)
import Level7 (game7)
import Level8 (game8)
import Text.Read (readMaybe)

-- import Assets (getBackgroundColor)
-- import Graphics.Gloss (display)
-- import Screens (getBackground, window, lv6)
-- import WeHateThisGame (Theme (LightTheme, DarkTheme), Block)

printWith :: String -> Theme -> IO()
printWith s _ = putStrLn s

run :: Int -> Theme -> IO()
run 0 t = game0 t
run 3 t = game3 t
run 4 t = game4 t
run 5 t = game5 t
run 6 t = game6 t
run 7 t = game7 t
run 8 t = game8 t
run m t =
    case m of
        1 -> do
            putStrLn "No level 1. Someone was being lazy!"
            runGame t
        2 -> do
            putStrLn "No level 2. Someone was being lazy!"
            runGame t
        _ -> do
            putStrLn "You're expecting more levels or what?"
            runGame t

runGame:: Theme -> IO()
runGame t = do 
    putStrLn "Input level number to play (0, 3-8): "
    level <- getLine
    case readMaybe level of
        Nothing -> do 
            putStrLn "Come again?"
            runGame t
        Just n -> run n DarkTheme

main :: IO ()
main = do
    putStrLn $
        "Let's play the low-effort graphical game " ++
        "that needs command line to start-"
    runGame DarkTheme