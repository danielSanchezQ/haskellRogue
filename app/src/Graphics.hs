module Graphics where

import Maps(getCell, getSize, Cell(..), Floor)
import Entities(Entity, getPosition, Race(..),getRace,getHealth, getName, getJob)
import Data.List(unfoldr, groupBy, sortBy, intersperse)
import Data.Map(toList)
import Utils(Pos, cutPadToWith)
import Logic
import Control.Monad


clearAndDraw :: (a->IO())-> a -> IO()
clearAndDraw f a = do
    replicateM_ 100 (putStrLn "")
    f a
    -- replicateM_ 20  (putStrLn "")

draw :: GameState -> IO()
draw gs = do
    putStrLn $ unlines $ drawGS (getHero gs : (getEnts gs)) (getMap gs)
    putStrLn ("hero position: "++ (show $ getPosition $ getHero gs))
    putStrLn ("\tLife: " ++ (show $ getHealth $ getHero gs))
    putStrLn ("Enemy: " ++ (concat $ intersperse " " $ map (show . getHealth) $ getEnts gs))
    putStrLn ("pos:   " ++ (concat $ intersperse " " $ map (show . getPosition) $ getEnts gs))
    putStrLn ("Turn: " ++ (show $ getTurnNumber gs) ++ "\tFloor Number: " ++ (show $ getFloorNumber gs))


-- ToDo: rewrite this in a nicer way
drawDeathScreen :: GameState -> IO()
drawDeathScreen gs = clearAndDraw drawDeathScreen' gs
drawDeathScreen' gs = do
    putStrLn  "\t --------------\t\t Your kills:"
    putStrLn  "\t/              \\"
    putStrLn ("\t|              |\t" ++ kill1)
    putStrLn ("\t|   R. I. P.   |\t" ++ kill2)
    putStrLn ("\t|              |\t" ++ kill3)
    putStrLn ("\t|  died at L" ++ floorNum ++ " |\t" ++ kill4)
    putStrLn ("\t|  on turn "  ++ turnNum  ++ " |\t" ++ kill5)
    putStrLn ("\t|              |\t" ++ kill6)
    foldM_ printLine "" kills
        where
            floorNum = cutPadToWith 2 ' ' $ show (getFloorNumber gs)
            turnNum = cutPadToWith 3 ' ' $ show (getTurnNumber gs)
            printLine :: String -> Entity -> IO(String)
            printLine a ent = do
                putStrLn ("\t|\t    |\t" ++ printKill ent)
                return "a"
            kill1 = if killNum > 0 then printKill $ head  $  getKillList gs else ""
            kill2 = if killNum > 1 then printKill $ (getKillList gs)!!1     else ""
            kill3 = if killNum > 2 then printKill $ (getKillList gs)!!2     else ""
            kill4 = if killNum > 3 then printKill $ (getKillList gs)!!3     else ""
            kill5 = if killNum > 4 then printKill $ (getKillList gs)!!4     else ""
            kill6 = if killNum > 5 then printKill $ (getKillList gs)!!5     else ""
            kills = if killNum > 6 then drop 2 (getKillList gs) else []
            printKill :: Entity -> String
            printKill ent = getName ent ++ " the " ++ (show $ getRace ent) ++ " " ++ (show $ getJob ent)
            killNum = length $ getKillList gs

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
drawCell Void      = ','
drawCell StairUp   = '<'
drawCell StairDown = '>'
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
