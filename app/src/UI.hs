module UI where
import Utils


data Action a = Action a | Choice Bool | None

parseCommand :: Char -> Action Move
parseCommand 'w' = Action UP
parseCommand 's' = Action DOWN
parseCommand 'a' = Action LEFT
parseCommand 'd' = Action RIGHT
parseCommand 'y' = Choice True
parseCommand 'n' = Choice False
parseCommand  _  = None

readCommand :: IO (Action Move)
readCommand = do 
    c <- getChar
    return (parseCommand c)
