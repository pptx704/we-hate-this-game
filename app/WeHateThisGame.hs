module WeHateThisGame where

-- | Blocks are to make game map
data Block = 
    Empty
    | JumpingBlock 
    | WallBlock 
    | NumberedBlock Int
    | Portal
    deriving Show

-- | There are two themes, dark and light
-- Dark themes have grey foreground and white background, vice versa for light
data Theme = 
    DarkTheme 
    | LightTheme

-- | Player coordinate and movement direction
type Player = (Float, Float, Movement)

-- | Movement type, will be used for improving collisions
-- and to implement gravity
data Movement = ToLeft | ToRight | Still

-- | Generalized game state
data State a = State Theme [[Block]] Player a Bool