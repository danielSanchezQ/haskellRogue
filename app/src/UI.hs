module UI where
import Control.Monad
import Utils        (Move(..))
import Graphics     (drawMenu)
import Data.Char    (digitToInt)


data Action a   = Action a  | None              deriving (Show, Eq)

instance Monad Action where
    return a          = Action a 
    (Action a)  >>= f = f a
    None        >>= _ = None


data Choice     = Accept    | Deny | Choice Int deriving (Show, Eq)

yesNoChoice     = [Accept, Deny]
numeralChoice   = [Choice n | n <- [0..9]]
allChoices      = yesNoChoice ++ numeralChoice

type Message    = String
type Menu       = Message


parseCommand :: Char -> Action Move
parseCommand 'w' = Action UP
parseCommand 's' = Action DOWN
parseCommand 'a' = Action LEFT
parseCommand 'd' = Action RIGHT
parseCommand  _  = None

parseChoice :: Char -> Action Choice
parseChoice 'y' = Action Accept
parseChoice 'n' = Action Deny
parseChoice  c  | c `elem` ['0'..'9']   = Action (Choice (digitToInt c))
                | otherwise             = None


--abstraction for taking commands, run with parse* to get an action. Daniel
readInput :: (Char -> Action a) -> Menu -> IO (Action a)
readInput rf m = do
    drawMenu m
    c <- getChar
    return (rf c)


readCommand = readInput parseCommand
readChoice  = readInput parseChoice

ask :: Eq a => Menu -> [a] -> (Menu -> IO(Action a)) -> IO(Action a)
ask m cs rf = do
    e <- rf m
    case e of
        None        -> ask m cs rf
        otherwise   -> if e `elem` [Action x | x <- cs] then return e else ask m cs rf
            


----ask "asndfoasidhfaspodifh" [Accept, Deny] readChoice

