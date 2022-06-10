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
        color (getBackgroundColor theme) (rectangleSolid 100 100),
        color (getForegroundColor theme) (rectangleSolid 98 98)
    ]

-- Wallblocks are jumpingblocks with texture.
wallBlock :: Theme -> Picture
wallBlock theme = pictures [jumpingBlock theme, texture]
    where
        line= color (getBackgroundColor theme) (rectangleSolid 20 5)
        threeLine = pictures [
            line,
            translate 30 0 line,
            translate (-30) 0 line
            ]
        twoLine = pictures [
            translate 15 0 line,
            translate (-15) 0 line
            ]
        texture = pictures [
            threeLine, 
            translate 0 30 twoLine, 
            translate 0 (-30) twoLine
            ]

-- Portal is a circle with yellow gradient. It doesn't depend on theme
portal :: Picture
portal = portal' [4, 16 .. 40] yellow
    where
        portal' [] _ = blank
        portal' (i:is) c = 
            pictures [portal' is (light c), color c (circleSolid i)]

-- Player is a static image for now. 
-- Maybe it will animate in the final submission
playerSprite :: Picture
playerSprite = pictures [head, body, hands, legs]
    where
        head = translate 0 120 (circleSolid 17.5)
        body = translate 0 60 (rectangleSolid 8 85)
        hands = translate 0 60 (leftHand <> rightHand)
        legs = translate 0 (-15) (leftLeg <> rightLeg)
        hand' = rectangleSolid 5 80
        leftHand = translate (-10) 0 (rotate 15 hand')
        rightHand = translate 10 0 (rotate (-15) hand')
        leg' = rectangleSolid 5 70
        leftLeg = translate (-5) 0 (rotate 5 leg')
        rightLeg = translate 5 0 (rotate (-5) leg')


-- Cage for pptx704's balloon lvl
cage :: Theme -> Picture
cage theme = pictures [
        frame, 
        translate (-25) 0 bars
        ]
    where
        frame = 
            pictures [
            jumpingBlock theme,
            color (getBackgroundColor theme) (rectangleSolid 90 90)
            ]
        bars = color (getForegroundColor theme) (bars' 3)
        bars' 0 = blank
        bars' n = rectangleSolid 5 98
            <> translate 25 0 (bars' (n-1))

rollingStone :: Theme -> Picture
rollingStone theme = color (getForegroundColor theme) (thickCircle 42.5 15)

-- Picture of a balloon for pptx704
balloon :: Picture 
balloon = pictures [circle, wire]
    where 
        circle = translate 0 60 (circleSolid 30)
        wire = rectangleSolid 2 100

-- A button container for the control boxes 
buttonBox :: Picture 
buttonBox = rectangleWire 25 25

-- An exit button 
exitButton :: Picture 
exitButton = pictures [box, exitIcon]
    where 
        box = translate 3 0 buttonBox
        line1 = rotate (45) (rectangleSolid 3 20)
        line2 = rotate (-45) (rectangleSolid 3 20)
        exitIcon = translate 3 (-0.5) (pictures [line1, line2])

-- A pause button 
pauseButton :: Picture 
pauseButton = pictures [box, pauseIcon]
    where 
        box = translate 3 0 buttonBox 
        line = rectangleSolid 3 20
        pauseIcon = translate 0.1 0 (pictures [line, translate 5 0 line])

-- change theme controller 
changeThemeButton :: Picture 
changeThemeButton = pictures [box, circle]
    where 
        box = translate 3 0 buttonBox 
        circle = translate 3 0 (circleSolid 7)