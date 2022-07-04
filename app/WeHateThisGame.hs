module WeHateThisGame where

-- | Blocks are to make game map
data Block = 
    Empty
    | JumpingBlock 
    | WallBlock 
    | NumberedBlock Int
    | Portal
    deriving (Show, Eq)

-- | There are two themes, dark and light
-- Dark themes have grey foreground and white background, vice versa for light
data Theme = 
    DarkTheme 
    | LightTheme
    deriving (Eq)

-- | Player coordinate and movement direction
type Player = (Float, Float, Movement, JumpDirection)

-- | Movement type, will be used for improving collisions
-- and to implement gravity
data Movement = ToLeft | ToRight | Still
data JumpDirection = ToUp Float Float | ToDown Float Float


-- | Generalized game state
data State a = State {
    getTheme :: Theme, 
    getGrid :: [[Block]], 
    getPlayer :: Player,
    getState :: a,
    getPortalState :: Bool,
    getGameState:: GameState
}

data GameState = Paused | Resumed | Over | Completed