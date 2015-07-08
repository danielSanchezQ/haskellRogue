module Utils where


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