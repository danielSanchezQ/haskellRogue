module Graphics where

import Maps(getCell, getSize, Cell(..), Floor)
import Entities(Entity, getPosition)
import Data.List(unfoldr, groupBy)
import Data.Map(toAscList)
import Utils(Pos)

draw :: [Entity] -> Floor -> [String]
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
drawCell Wall      = '#'
drawCell Empty     = '.'
drawCell Window    = 'x'
drawCell Door      = '+'
drawCell _         = 'X'

drawMap :: Floor -> [[Char]]
drawMap (size, floor) = map (map (drawCell . snd)) $ groupBy sameLine $ toAscList floor
    where
        sameLine :: (Pos, Cell) -> (Pos, Cell) -> Bool
        sameLine = (\a b -> (fst $ fst a) == (fst $ fst b))
