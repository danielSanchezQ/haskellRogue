{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
-- module Maps (Floor(), getSize, getCell, Cell(..),generateMap, standardMap) where
module Maps where

import Utils(Pos)
import qualified Data.Map.Strict as Map
import System.Random(RandomGen,randomR,Random,split,random)
import Data.Tuple(swap)

-- height and width of the rooms/tiles that make up a floor
roomSide = 12

data Cell = Wall | Empty | Door | StairDown | StairUp | Window | Exit | Void
    deriving (Show, Read, Eq)

-- | size as Pos, Map of all cells with coordinates as Pos
type Floor = (Pos, Map.Map Pos Cell)

instance Random Floor where
    randomR _ = random
    random ranGen = (standardMap gen1, lastGen)
        where
            (gen1, lastGen) = split ranGen

-- | generate standard map
standardMap :: (RandomGen g) => g -> Floor
standardMap ranGen = placeStairs ran1 $generateMap ran2 (4,3)
    where
        (ran1,ran2) = split ranGen

-- | generate a floor with specific size
--   generateMap takes a seed and a size as Pos
generateMap :: RandomGen g => g -> Pos -> Floor
generateMap ran (x,1) = generateRow ran x
generateMap ranGen (x,y) = roomMergeV ran1 (generateRow ran1 x) (generateMap ran2 (x,y-1))
    where
        (ran1,ran') = split ranGen
        (ran2,ran3) = split ran'

changeCell :: Pos -> Cell -> Floor -> Floor
changeCell pos newCell floor@(size, cellMap)
    | isPositionValid floor pos = (size, Map.insert pos newCell cellMap)
    | otherwise = floor

placeStairs :: RandomGen g => g -> Floor -> Floor
placeStairs ranGen floor = changeCell ranPos StairDown floor
    where
        ranPos = getRandomEmpty ranGen floor

generateRow :: RandomGen g => g -> Int -> Floor
generateRow ranGen 1 = generateRoom ranGen roomSide roomSide
generateRow ranGen n = roomMergeH ran3 (generateRoom ran1 roomSide roomSide) (generateRow ran2 (n-1))
    where
        (ran1,ran') = split ranGen
        (ran2,ran3) = split ran'

rowsMerge :: RandomGen g => g -> [Floor] -> Floor
rowsMerge _ (floor:[]) = floor
rowsMerge ranGen (floor1: fs) = roomMergeV ran1 floor1 $ rowsMerge ran2 fs
    where
        (ran1, ran2) = split ranGen

roomMergeV :: RandomGen g => g -> Floor -> Floor -> Floor
roomMergeV ranGen (p1, room1) (p2, room2) = (addPairs p1 p2, connectRooms seqRooms)
    where
        addPairs (x1, y1) (x2, y2) = (x1, y1+y2)
        seqRooms = Map.union room1 (Map.mapKeys shiftKeys room2)
        shiftKeys :: Pos -> Pos
        shiftKeys (x,y) = (x,roomSide+y)
        connectRooms = makeFloor . makeWalls
        makeFloor = Map.unionWithKey placeDoors floors
        floors = Map.fromList $ [((corridorX,y),Empty) | y <- halfToHalf]
        corridorX :: Int
        corridorX = (div roomSide 2) -1 + (fst $ randomR(0,2) ranGen) + (roomSide * (fst (randomR (0,(div (fst p1) roomSide)-1) ranGen)))
        halfToHalf = [(roomSide `div` 2)+1..(roomSide `div` 2) + roomSide - 1]
        placeDoors :: Pos -> Cell -> Cell -> Cell
        placeDoors (a,b) newCell oldCell
            | oldCell == Wall && ran= Door
            | otherwise             = Empty
                where
                    -- 30 % of connections will have doors
                    ran = (mod ((fst $ randomR (0,10) ranGen)+a+b) 10) > 6
        makeWalls = Map.unionWith (prefereEmpty) walls
        walls = Map.fromList $ [((x,y),Wall) | x<- [corridorX-1,corridorX+1], y<-halfToHalf]
        prefereEmpty newCell oldCell
            | oldCell == Empty      = Empty
            | oldCell == Door       = Door
            | otherwise             = newCell

rotateMap :: Floor -> Floor
rotateMap (size, cells) = (swap size, Map.mapKeys swap cells)

roomMergeH ranGen r1 r2 = rotateMap $ roomMergeV ranGen (rotateMap r1) (rotateMap r2)

generateRoom :: RandomGen g => g -> Int -> Int -> Floor
generateRoom ranGen xmax ymax = ((xmax, ymax), Map.fromList $ zip (makeGrid xmax ymax) $ concat cells)
    where
        (x,rg2) = (randomR (1,3) ranGen)
        (y,rg3) = (randomR (1,3) rg2)
        (w,rg4) = (randomR (7,xmax-x-1) rg3)
        (h,_) = (randomR (7,ymax-y) rg4)
        cells = take ymax $ voidTop ++ [edge] ++ (replicate (h-2) middle) ++ [edge] ++ (repeat voidLine)
        voidTop :: [[Cell]]
        voidTop = replicate y voidLine
        voidLine :: [Cell]
        voidLine = void xmax
        void :: Int -> [Cell]
        void n = replicate n Void
        edge = take xmax $ void x ++ replicate w Wall ++ repeat Void
        middle = take xmax $ void x ++  Wall : (take (w-2) $ repeat Empty) ++ [Wall] ++ voidLine

isPositionValid :: Floor -> Pos -> Bool
isPositionValid floor pos = isValidPos pos
    where
        isValidPos (x,y) = (x>0) && (y>0) && (x<=xmax) && (y<=ymax)
        (xmax, ymax) = getSize floor

isPositionWalkable :: Floor -> Pos -> Bool
isPositionWalkable floor pos = isPositionValid floor pos &&
                            elem cell [Door, Empty, StairDown, StairUp]
                        where
                            cell = getCell floor pos

-- | returns a random Pos that points to a Walkable cell
getRandomEmpty :: RandomGen g => g -> Floor -> Pos
getRandomEmpty ranGen floor@(size,cells)
    | isThereNone   = error ("no empty place on the map")
    | isGood        = newPos
    | otherwise     = getRandomEmpty newRan newFloor
        where
            isThereNone = length allEmpties == 0
            isGood = isPositionWalkable floor newPos
            newPos = allEmpties!!(fst $ randomR(0,length allEmpties) ran1)
            allEmpties :: [Pos]
            allEmpties = Map.keys $ Map.filter isWalkable cells
            (newRan,ran1) = split ranGen
            newFloor :: Floor
            newFloor = (size, Map.filter isWalkable cells)
            isWalkable cell = elem cell [Door, Empty, StairDown, StairUp]

-- | get the Cell at Pos
getCell :: Floor -> Pos -> Cell
getCell (_, f) pos = Map.findWithDefault Void pos f

-- | get the size of a Floor in Pos
getSize :: Floor -> Pos
getSize (size, _) = size

makeGrid :: Int -> Int -> [(Int, Int)]
makeGrid w h = concat $ take h $ map (uncurry zip) $ zip ((map (take w . repeat) [1..h])) (repeat [1..w])
