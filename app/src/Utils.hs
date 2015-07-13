module Utils where


data Move = UP | DOWN | LEFT | RIGHT deriving (Show, Eq)
--data Pos = Pos Int Int Int
type Pos = (Int, Int)
type Direction = Move -- used in a move HeroAction, conceptually same as Move?

movePos :: Pos -> Pos -> Pos
movePos (x, y) (x', y') = (x+x', y+y')

up, down, left, right :: Pos -> Int -> Pos
up      (x, y) n = (x,   y+n)
down    (x, y) n = (x,   y-n)
left    (x, y) n = (x-n, y)
right   (x, y) n = (x+n, y)

up', down', left', right' :: Pos -> Pos
up'     p = up      p 1
down'   p = down    p 1
left'   p = left    p 1
right'  p = right   p 1

makeMove :: Pos -> Move -> Int -> Pos
makeMove p UP    n =  up    p n
makeMove p DOWN  n =  down  p n
makeMove p LEFT  n =  left  p n
makeMove p RIGHT n =  right p n

makeMove' :: Pos -> Move -> Pos
makeMove' p UP    =  up'    p
makeMove' p DOWN  =  down'  p
makeMove' p LEFT  =  left'  p
makeMove' p RIGHT =  right' p

moveToPos :: Move -> Pos
moveToPos UP    = (0,-1)
moveToPos DOWN  = (0, 1)
moveToPos RIGHT = (1, 0)
moveToPos LEFT  = (-1, 0)

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
