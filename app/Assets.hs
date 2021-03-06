module Assets where
import WeHateThisGame
import Graphics.Gloss

-- getZOrderColor functions are for adjusting themes
getForegroundColor :: Theme -> Color
getForegroundColor LightTheme = light black
getForegroundColor DarkTheme = white

getBackgroundColor :: Theme -> Color
getBackgroundColor LightTheme = white
getBackgroundColor DarkTheme = light (light black)

-- Initially jumping blocks are empty. For some specific level, they might
-- have some texture
jumpingBlock :: Theme -> Picture
jumpingBlock theme = pictures [
        color (getForegroundColor theme) (rectangleSolid 100 100),
        color (getBackgroundColor theme) (rectangleSolid 98 98)
    ]

-- Wallblocks are jumpingblocks with texture.
wallBlock :: Theme -> Picture
wallBlock theme = pictures [jumpingBlock theme, texture]
    where
        line_ = color (getForegroundColor theme) (rectangleSolid 20 5)
        threeLine = pictures [
            line_,
            translate 30 0 line_,
            translate (-30) 0 line_
            ]
        twoLine = pictures [
            translate 15 0 line_,
            translate (-15) 0 line_
            ]
        texture = pictures [
            threeLine, 
            translate 0 30 twoLine, 
            translate 0 (-30) twoLine
            ]

-- Numbered blocks are jumping blocks but with a number on them
numberedBlock :: Theme ->  Int -> Picture
numberedBlock t a 
    = jumpingBlock t <> color fgcolor (translate (-20) (-20) (scale 0.5 0.5 (Text (show a))))
    where
        fgcolor = getForegroundColor t

-- Draws blocks of different types
drawBlock :: Theme -> Block -> Picture
drawBlock _ Empty = blank
drawBlock t WallBlock = wallBlock t
drawBlock t JumpingBlock = jumpingBlock t
drawBlock t (NumberedBlock a) = numberedBlock t a
drawBlock _ Portal = portal

-- Portal is a circle with yellow gradient. It doesn't depend on theme
portal :: Picture
portal = portal' [4, 16 .. 40] yellow
    where
        portal' [] _ = blank
        portal' (i:is) c = 
            portal' is (light c) <> color c (circleSolid i)

-- Player is a static image for now. marker
-- Maybe it will animate in the final submission
playerSprite :: Theme -> Player -> Picture
playerSprite theme (x, y, _, _) = translate x y (pictures [head_, body, hands, legs])
    where
        fgcolor = getForegroundColor theme
        head_ = translate 0 120 (color fgcolor (circleSolid 17.5))
        body = translate 0 60 (color fgcolor (rectangleSolid 8 85))
        hands = translate 0 60 (leftHand <> rightHand)
        legs = translate 0 (-15) (leftLeg <> rightLeg)
        hand' = color fgcolor (rectangleSolid 5 80)
        leftHand = translate (-10) 0 (rotate 15 hand')
        rightHand = translate 10 0 (rotate (-15) hand')
        leg' = color fgcolor (rectangleSolid 5 70)
        leftLeg = translate (-5) 0 (rotate 5 leg')
        rightLeg = translate 5 0 (rotate (-5) leg')

-- Cage for pptx704's balloon lvl
cage :: Theme -> Picture
cage theme = color (getForegroundColor theme) frame
    where
        frame = upperBar <> lowerBar <> leftBar <> rightBar
        upperBar = translate 0 48 horizontalBar
        lowerBar = translate 0 (-48) horizontalBar
        leftBar = translate (-48) 0 verticalBar
        rightBar = translate 48 0 verticalBar
        horizontalBar = rectangleSolid 100 4
        verticalBar = rectangleSolid  4 100

-- | Stones for lv 6. Some other level will also use it.
rollingStone :: Theme -> Picture
rollingStone theme = color (getForegroundColor theme) (thickCircle 42.5 10)

-- Picture of a balloon for level 3
balloon :: Theme -> Picture 
balloon t = color (getForegroundColor t) (circle_ <> wire)
    where 
        -- _ to differentiate from circle
        circle_ = circleSolid 45
        wire = translate 0 (-195) $ rectangleSolid 2 300