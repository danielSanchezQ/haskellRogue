module Maps (Floor(), getSize, getCell, Cell(..),generateMap, standardMap) where

import Utils(Pos)
import qualified Data.Map.Strict as Map

data Cell = Wall | Empty | Door | StairDown | StairUp | Window | Exit
    deriving (Show, Read)

-- | size as Pos, Map of all cells with coordinates as Pos
type Floor = (Pos, Map.Map Pos Cell)

-- | generate standard map
standardMap = generateMap (repeat 0) 20 10

-- | generate a floor with specific size
generateMap :: [Int] -> Int -> Int -> Floor
generateMap rs = generateRoom

generateRoom :: Int -> Int -> Floor
generateRoom w h = ((w, h), Map.fromList $ zip (makeGrid w h) cells)
    where
        cells :: [Cell]
        cells = concat $ [edge] ++ (take (h-2) $ repeat middle) ++ [edge]
        edge = take w $ repeat Wall
        middle = Wall : (take (w-2) $ repeat Empty) ++ [Wall]

addExit :: Int -> Int -> Floor -> Floor
addExit = undefined

-- | get the Cell at Pos
getCell :: Floor -> Pos -> Cell
getCell (_, f) pos = Map.findWithDefault Empty pos f

-- | get the size of a Floor in Pos
getSize :: Floor -> Pos
getSize (size, _) = size

makeGrid :: Int -> Int -> [(Int, Int)]
makeGrid w h = concat $ take h $ map (uncurry zip) $ zip ((map (take w . repeat) [1..h])) (repeat [1..w])
