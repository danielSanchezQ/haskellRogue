module UI where
import Utils


data Action a   = Action a  | None              deriving (Show)
data Choice     = Accept    | Deny | Choice Int deriving (Show)

parseCommand :: Char -> Action Move
parseCommand 'w' = Action UP
parseCommand 's' = Action DOWN
parseCommand 'a' = Action LEFT
parseCommand 'd' = Action RIGHT
parseCommand  _  = None

parseChoice :: Char -> Action Choice
parseChoice 'y' = Action Accept
parseChoice 'n' = Action Deny
parseChoice  c  | read c::Int in [1..] = Action Choice (chartoInt)

readInput :: (Char -> Action a) -> IO (Action a)
readInput rf = do 
    c <- getChar
    return (rf c)

