module UI where
import Utils


data Action a   = Action a  | None
data Choice     = Accept    | Deny

parseCommand :: Char -> Action Move
parseCommand 'w' = Action UP
parseCommand 's' = Action DOWN
parseCommand 'a' = Action LEFT
parseCommand 'd' = Action RIGHT
parseCommand  _  = None

parseChoice :: Char -> Action Choice
parseChoice 'y' = Action Accept
parseChoice 'n' = Action Deny
parseChoice  _  = None

readInput :: (Char -> Action a) -> IO (Action a)
readInput rf = do 
    c <- getChar
    return (rf c)