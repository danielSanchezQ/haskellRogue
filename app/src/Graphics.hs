module Graphics where

import Maps
import Entities(Entity,getPosition)

draw :: [Entity] -> Map -> [String]
draw es m = overlayEs (map changeEs es) $ drawMap m

overlayEs :: [(Int, Int, Char)] -> [[Char]] -> [String]
overlayEs es m = zipWith' overlayRow unlinedEs m
    where
        unlinedEs :: [[(Int, Char)]]
        unlinedEs = uEs 1 sortedEs
        uEs :: Int ->[(Int,Int,Char)] -> [[(Int,Char)]]
        uEs n [] = []
        uEs n allEs@((x, a, b):es)
            | n == x    = [(a,b)] : uEs n es
            | otherwise = []++uEs (n+1) allEs
        sortedEs = es

--zipWith that doesnt stop if the first list is empty (too short)
--ToDo: really unoptimized
zipWith' f as bs = zipWith f as bs ++ restOfBs
    where
        restOfBs = drop (length as) bs

overlayRow :: [(Int, Char)] -> String -> String
overlayRow = undefined

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
