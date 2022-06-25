module WeHateThisGame where
import Graphics.Gloss

-- WallBlock are for walls and PlainBlock is for jumping
data Block = 
    JumpingBlock 
    | WallBlock 
    | NumberedBlock Int

-- There are two themes, dark and light
-- Dark themes have grey foreground and white background, vice versa for light
data Theme = 
    DarkTheme 
    | LightTheme