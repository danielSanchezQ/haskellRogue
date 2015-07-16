-- module Maps (Floor(), getSize, getCell, Cell(..),generateMap, standardMap) where
module Maps where

import Utils(Pos)
import qualified Data.Map.Strict as Map
import System.Random--(RandomGen,randomR)

data Cell = Wall | Empty | Door | StairDown | StairUp | Window | Exit | Void
    deriving (Show, Read, Eq)

-- | size as Pos, Map of all cells with coordinates as Pos
type Floor = (Pos, Map.Map Pos Cell)

-- | generate standard map
standardMap :: (RandomGen g) => g -> Floor
standardMap ranGen = generateMap ranGen (4,3)

-- | generate a floor with specific size
--   generateMap takes a seed and a size as Pos
generateMap :: RandomGen g => g -> Pos -> Floor
generateMap ran (x,1) = generateRow ran x
generateMap ranGen (x,y) = roomMergeV (generateRow ran1 x) (generateMap ran2 (x,y-1))
    where
        (ran1,ran2) = split ranGen

generateRow :: RandomGen g => g -> Int -> Floor
generateRow ranGen 1 = generateRoom ranGen 10 10
generateRow ranGen n = roomMergeH (generateRoom ran1 10 10) (generateRow ran2 (n-1))
    where
        (ran1,ran2) = split ranGen

rowsMerge :: [Floor] -> Floor
rowsMerge (floor:[]) = floor
rowsMerge (floor1: fs) = roomMergeV floor1 $ rowsMerge fs

roomMergeV :: Floor -> Floor -> Floor
roomMergeV (p1, room1) (p2, room2) = (addPairs p1 p2, connectRooms seqRooms)
    where
        addPairs (x1, y1) (x2, y2) = (x1, y1+y2)
        seqRooms = Map.union room1 (Map.mapKeys shiftKeys room2)
        shiftKeys :: Pos -> Pos
        shiftKeys (x,y) = (x,10+y)
        connectRooms = makeFloor . makeWalls
        makeFloor = Map.unionWith placeDoors floors
        floors = Map.fromList $ [((5,y),Empty) | y <- [5..15]]
        placeDoors newCell oldCell
            | oldCell == Wall       = Door
            | otherwise             = Empty
        makeWalls = Map.unionWith (prefereEmpty) walls
        walls = Map.fromList $ [((x,y),Wall) | x<- [4,6], y<-[5..15]]
        prefereEmpty newCell oldCell
            | oldCell == Empty      = Empty
            -- | oldCell == Wall       = Door
            | otherwise             = newCell

roomMergeH :: Floor -> Floor -> Floor
roomMergeH (p1, room1) (p2, room2) = (addPairs p1 p2, connectRooms seqRooms)
    where
        addPairs (x1, y1) (x2, y2) = (x1+x2, y1)
        seqRooms = Map.union room1 (Map.mapKeys shiftKeys room2)
        shiftKeys :: Pos -> Pos
        shiftKeys (x,y) = (10+x,y)
        connectRooms = makeFloor . makeWalls
        makeFloor = Map.union floors
        floors = Map.fromList $ [((x,5),Empty) | x <- [5..15]]
        makeWalls = Map.unionWith (prefereEmpty) walls
        walls = Map.fromList $ [((x,y),Wall) | y<- [4,6], x<-[5..15]]
        prefereEmpty newCell oldCell
            | oldCell == Empty      = Empty
            -- | oldCell == Wall       = Door
            | otherwise             = newCell

generateRoom :: RandomGen g => g -> Int -> Int -> Floor
generateRoom ranGen xmax ymax = ((xmax, ymax), Map.fromList $ zip (makeGrid xmax ymax) $ concat cells)
    where
        (x,rg2) = (randomR (1,3) ranGen)
        (y,rg3) = (randomR (1,3) rg2)
        (w,rg4) = (randomR (5,xmax-x) rg3)
        (h,_) = (randomR (5,ymax-y) rg4)
        cells = take ymax $ voidTop ++ [edge] ++ (replicate (h-2) middle) ++ [edge] ++ (repeat voidLine)
        voidTop :: [[Cell]]
        voidTop = replicate y voidLine
        voidLine :: [Cell]
        voidLine = void xmax
        void :: Int -> [Cell]
        void n = replicate n Void
        edge = take xmax $ void x ++ replicate w Wall ++ repeat Void
        middle = take xmax $ void x ++  Wall : (take (w-2) $ repeat Empty) ++ [Wall] ++ voidLine

addExit :: Int -> Int -> Floor -> Floor
addExit = undefined

-- | get the Cell at Pos
getCell :: Floor -> Pos -> Cell
getCell (_, f) pos = Map.findWithDefault Void pos f

-- | get the size of a Floor in Pos
getSize :: Floor -> Pos
getSize (size, _) = size

makeGrid :: Int -> Int -> [(Int, Int)]
makeGrid w h = concat $ take h $ map (uncurry zip) $ zip ((map (take w . repeat) [1..h])) (repeat [1..w])
