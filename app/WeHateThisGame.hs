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
-- data State a = State {
--     getTheme :: Theme, 
--     getGrid :: [[Block]], 
--     getPlayer :: Player,
--     getState :: a
--     getPortalState :: Bool,
--     getGameState:: GameState
-- }

data State = State {
    getTheme :: Theme,
    getGrid :: [[Block]],
    getPlayer :: Player,
    getLevel :: LevelState,
    getPortalState :: Bool,
    getGameState :: GameState
}
-- | Wrappers for each level to generalize all level states 
data LevelState = Lv0 Block | Lv3 Balloon | Lv4 ([Ball], Int) | Lv5 Block
    | Lv6 [Stone] | Lv7 Int | Lv8 ([Int], [Int])

-- | Keeps track of the current game state
data GameState = Paused | Resumed | Over | Completed

-- | The following are level specific type declarations

-- Balloon = Coordinated Caged? 
-- Level 3
type Balloon = ((Float, Float), Bool)

-- Data type to store ball position
-- Level 4
type Ball = (Float, Float)

-- | (x, y, rot, char)
-- Level 6
type Stone = (Float, Float, Float, Char)