module Graphics where

import Maps
import Entities(Entity,getPosition)

draw :: [Entity] -> Map -> [String]
draw es m = foldr overlayEs emptyMap es
    where
        emptyMap = drawMap m

-- ugly and inefficient, ToDo: change to cps or anything better. skapazzo
-- ToDo: check for out of bounds coordinates
overlayEs :: Entity -> [String] -> [String]
overlayEs e cs = take (x-1) cs ++ (overLayEs2 y (cs!!x): drop x cs)
    where
        (x,y) = getPosition e
        overLayEs2 :: Int -> String -> String
        overLayEs2 y cs = take (x-1) cs ++ (char : drop x cs)
        char = drawEntity e

changeEs :: Entity -> (Int, Int, Char)
changeEs e = let (x,y) = getPosition e in (x, y, drawEntity e)

drawEntity :: Entity -> Char
drawEntity _ = '@'

drawCell :: Cell -> Char
drawCell W      = '#'
drawCell F      = '.'
drawCell _      = 'X'

drawMap :: Map -> [[Char]]
drawMap = map (map drawCell)
