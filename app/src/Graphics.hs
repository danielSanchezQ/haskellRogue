module Graphics where

import Maps
import Entities(Entity)

draw :: [Entity] -> Map -> [String]
draw es m = overlayEs (map changeEs es) $ drawMap m

overlayEs :: [(Int, Int, Char)] -> [[Char]] -> [String]
overlayEs = undefined

changeEs :: Entity -> (Int, Int, Char)
changeEs =  undefined

drawEntity :: Entity -> Char
drawEntity _ = '@'

drawCell :: Cell -> Char
drawCell W      = '#'
drawCell F      = '.'
drawCell _      = 'X'

drawMap :: Map -> [[Char]]
drawMap = map (map drawCell)
