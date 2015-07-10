module UI where
import Utils(Move(..))
import Data.Char(digitToInt)


data Action a   = Action a  | None              deriving (Show)
data Choice     = Accept    | Deny | Choice Int deriving (Show)

yesNoChoice     = [Accept, Deny]
numeralChoice   = [Choice n | n <- [0..9]]
allChoices      = yesNoChoice ++ numeralChoice

type Message    = String
type Menu       = [Message]


parseCommand :: Char -> Action Move
parseCommand 'w' = Action UP
parseCommand 's' = Action DOWN
parseCommand 'a' = Action LEFT
parseCommand 'd' = Action RIGHT
parseCommand  _  = None

parseChoice :: Char -> Action Choice
parseChoice 'y' = Action Accept
parseChoice 'n' = Action Deny
parseChoice  c  | digitToInt c `elem` [0..9] = Action (Choice (digitToInt c))


--abstraction for taking commands, run with parse* to get an action. Daniel
readInput :: (Char -> Action a) -> IO (Action a)
readInput rf = do 
    c <- getChar
    return (rf c)