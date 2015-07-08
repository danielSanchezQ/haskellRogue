module Graphics where

import Maps

-- draw :: GameState -> [String]
draw = undefined

drawCell :: Cell -> String
drawCell W      = "."
drawCell F      = "#"
drawCell _      = "X"

drawMap :: Map -> [String]
drawMap = map drawCell


