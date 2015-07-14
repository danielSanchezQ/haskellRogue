module Graphics where

import Maps(getCell, getSize, Cell(..), Floor)
import Entities(Entity, getPosition, Race(..),getRace,getHealth)
import Data.List(unfoldr, groupBy, sortBy)
import Data.Map(toList)
import Utils(Pos)
import Logic

draw :: GameState -> IO()
draw gs = do
    putStrLn $ unlines $ drawGS (getHero gs : (getEnts gs)) (getMap gs)
    -- putStrLn ("hero position: "++ (show $ getPosition $ getHero gs) ++
              -- "\tLife: " ++ (show $ getHealth $ getHero gs))

drawGS :: [Entity] -> Floor -> [String]
drawGS es m = foldr overlayEs emptyMap es
    where
        emptyMap = drawMap m

-- ugly and inefficient, ToDo: change to cps or anything better. skapazzo
-- ToDo: check for out of bounds coordinates
overlayEs :: Entity -> [String] -> [String]
overlayEs e lines = take (y-1) lines ++ (overLayEs2 x (lines!!(y-1)): drop y lines)
    where
        (x,y) = getPosition e
        overLayEs2 :: Int -> String -> String
        overLayEs2 x cells = take (x-1) cells ++ (char : drop x cells)
        char = drawEntity e

changeEs :: Entity -> (Int, Int, Char)
changeEs e = let (x,y) = getPosition e in (x, y, drawEntity e)

entitiesTable :: [(Race, Char)]
entitiesTable = [(Human,    'H'),
                 (Hero,     '@'),
                 (Troll,    'T'),
                 (Dragon,   'D'),
                 (Elven,    'e'),
                 (Dwarven,  'd'),
                 (Feline,   'f')]

drawEntity :: Entity -> Char
drawEntity e = case (lookup (getRace e) entitiesTable) of
        Nothing -> 'D'
        Just char -> char

drawCell :: Cell -> Char
drawCell Wall      = '#'
drawCell Empty     = '.'
drawCell Window    = 'x'
drawCell Door      = '+'
drawCell _         = 'X'

drawMap :: Floor -> [[Char]]
drawMap (size, floor) = map (map (drawCell . snd)) $ groupBy sameLine $ sortBy sorting $ toList floor
    where
        sameLine :: (Pos, Cell) -> (Pos, Cell) -> Bool
        sameLine = (\a b -> (snd $ fst a) == (snd $ fst b))
        sorting :: (Pos, Cell) -> (Pos, Cell) -> Ordering
        sorting ((x1,y1),_) ((x2,y2),_)
            | y1 > y2       = GT
            | y1 < y2       = LT
            | x1 > x2       = GT
            | x1 < x1       = LT
            | otherwise     = EQ

drawMenu :: String -> IO ()
drawMenu m = putStrLn m

-- drawGameState :: GameState -> [[Char]]
-- drawGameState gs = draw ents (world gs)
--     where
--         ents = hero gs : (entities gs)
