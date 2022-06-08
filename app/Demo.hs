module Demo where

import Graphics.Gloss

window :: Display
window = InWindow "Random window" (200, 200) (100, 100)

runner :: IO()
runner = display window blue (Circle 80)