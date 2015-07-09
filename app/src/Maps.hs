module Maps (Floor(), getSize, getCell, Cell(..),generateMap, standardMap, exampleMap) where

import Utils(Pos)
import Math.Geometry.Grid

data Cell = Wall | Empty | Door | StairDown | StairUp | Window | Exit
    deriving (Show, Read)

type Floor = [[Cell]]

-- | generate standard map
standardMap = generateMap (repeat 0) 10 20

-- | constant example floor for debugging
w = Wall
f = Empty
exampleMap :: Floor
exampleMap = [[w,w,w,w,w],[w,f,f,f,w],[w,f,f,f,w],[w,w,w,w,w]]

-- | generate a floor with specific size
generateMap :: [Int] -> Int -> Int -> Floor
generateMap rs = generateRoom z
    where
        z = head rs

generateRoom ::Int -> Int -> Int -> Floor
generateRoom _ h w = edge : (take (h-2) $ repeat middle) ++ [edge]
    where
        edge = take w $ repeat Wall
        middle = Wall : (take (w-2) $ repeat Empty) ++ [Wall]

addExit :: Int -> Int -> Floor -> Floor
addExit = undefined

-- | get the Cell at Pos
getCell :: Floor -> Pos -> Cell
getCell fl (x,y) = fl!!x!!y

-- | get the size of a Floor in Pos
getSize :: Floor -> Pos
getSize fl = (length fl, length $ head fl)

