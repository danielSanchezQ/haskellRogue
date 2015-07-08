module Utils where


data Move = UP | DOWN | LEFT | RIGHT
--data Pos = Pos Int Int Int
type Pos = (Int, Int)

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