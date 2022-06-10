module WeHateThisGame where

-- WallBlock are for walls and PlainBlock is for jumping
data Block = WallBlock | PlainBlock

-- There are two themes, dark and light
-- Dark themes have grey foreground and white background, vice versa for light
data Theme = DarkTheme | LightTheme