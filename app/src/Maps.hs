module Maps (Floor(), getSize, getCell, Cell(..),generateMap, standardMap) where

import Utils(Pos)
import qualified Data.Map.Strict as Map

data Cell = Wall | Empty | Door | StairDown | StairUp | Window | Exit | Void
    deriving (Show, Read, Eq)

-- | size as Pos, Map of all cells with coordinates as Pos
type Floor = (Pos, Map.Map Pos Cell)

-- | generate standard map
standardMap :: Int -> Floor
standardMap seed = generateMap seed (20,10)

-- | generate a floor with specific size
--   generateMap takes a seed and a size as Pos
generateMap :: Int -> Pos -> Floor
generateMap seed (x,y) = generateRoom seed x y

generateRoom :: Int -> Int -> Int -> Floor
generateRoom s w h = ((w, h), Map.fromList $ zip (makeGrid w h) cells)
    where
        cells :: [Cell]
        cells = concat $ [edge] ++ (take (w-2) $ repeat middle) ++ [edge]
        edge = take h $ repeat Wall
        middle = Wall : (take (h-2) $ repeat Empty) ++ [Wall]

addExit :: Int -> Int -> Floor -> Floor
addExit = undefined

-- | get the Cell at Pos
getCell :: Floor -> Pos -> Cell
getCell (_, f) pos = Map.findWithDefault Void pos f

-- | get the size of a Floor in Pos
getSize :: Floor -> Pos
getSize (size, _) = size

makeGrid :: Int -> Int -> [(Int, Int)]
makeGrid w h = concat $ take w $ map (uncurry zip) $ zip ((map (take h . repeat) [1..w])) (repeat [1..h])
