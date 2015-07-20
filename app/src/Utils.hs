module Utils where

-- ToDo UP and Down are dangerous(see incomplete pattern matches below)!! do we want to keep them here?
data Move = NORTH | SOUTH | WEST | EAST | STAY | UP | DOWN deriving (Show, Eq)
--data Pos = Pos Int Int Int
type Pos = (Int, Int)
type Direction = Move -- used in a move HeroAction, conceptually same as Move?

reverseMove :: Move -> Move
reverseMove NORTH      = SOUTH
reverseMove SOUTH    = NORTH
reverseMove WEST    = EAST
reverseMove EAST   = WEST

movefromPos :: Pos -> Pos -> Move
movefromPos (x1, y1) (x2, y2)   | x1 >= x2  && y1 > y2 = if xdif > ydif then WEST     else SOUTH
                                | x1 >= x2  && y1 < y2 = if xdif > ydif then WEST     else NORTH
                                | x1 <  x2  && y1 > y2 = if xdif > ydif then EAST    else SOUTH
                                | x1 <  x2  && y1 < y2 = if xdif > ydif then EAST    else NORTH
                                | otherwise            = STAY
                                where
                                    xdif = abs $ x1 - x2
                                    ydif = abs $ y1 - y2


movePos :: Pos -> Pos -> Pos
movePos (x, y) (x', y') = (x+x', y+y')

up, down, left, right :: Pos -> Int -> Pos
up      (x, y) n = (x,   y-n)
down    (x, y) n = (x,   y+n)
left    (x, y) n = (x-n, y)
right   (x, y) n = (x+n, y)

up', down', left', right' :: Pos -> Pos
up'     p = up      p 1
down'   p = down    p 1
left'   p = left    p 1
right'  p = right   p 1

makeMove :: Pos -> Move -> Int -> Pos
makeMove p NORTH    n =  up    p n
makeMove p SOUTH  n =  down  p n
makeMove p WEST  n =  left  p n
makeMove p EAST n =  right p n
makeMove p STAY  _ =  p

makeMove' :: Pos -> Move -> Pos
makeMove' p NORTH    =  up'    p
makeMove' p SOUTH  =  down'  p
makeMove' p WEST  =  left'  p
makeMove' p EAST =  right' p
makeMove' p STAY  =  p

moveToPos :: Move -> Pos
moveToPos NORTH    = (0,-1)
moveToPos SOUTH  = (0, 1)
moveToPos EAST = (1, 0)
moveToPos WEST  = (-1,0)
moveToPos STAY  = (0, 0)


boundValue :: Int-> Int -> Int -> Int
boundValue v max min    | v > max   = max
                        | v < min   = min
                        | otherwise = v

checkRange :: Pos -> Pos -> Int -> Bool
checkRange (x1, y1) (x2, y2) r = (round . sqrt $ fromIntegral xysum) <= r
    where
        x'      = (x1 - x2) ^ 2
        y'      = (y1 - y2) ^ 2
        xysum   = x' + y'

cutPadToWith :: Int -> a -> [a] -> [a]
cutPadToWith num space list
    | length list == num    = take num list
    | otherwise             = list ++ (replicate (num - length list) space)
