module WeHateThisGame where

-- WallBlock are for walls and PlainBlock is for jumping
data Block = 
    Empty
    | JumpingBlock 
    | WallBlock 
    | NumberedBlock Int
    deriving Show
-- There are two themes, dark and light
-- Dark themes have grey foreground and white background, vice versa for light
data Theme = 
    DarkTheme 
    | LightTheme
    
type Player = (Float, Float)

data Movement = ToUp | ToLeft | ToRight

data State a = State Theme [[Block]] Player a Bool