module UI where
import Control.Monad
import Utils        (Move(..),Direction,Pos)
import Graphics     (drawMenu)
import Data.Char    (digitToInt)
import Logic --hiding (TurnAction)


-- pasted from old logic to make it compile
-- data Action a   = Act a  | None   deriving (Show, Eq)

-- data Choice     = Accept    | Deny | Choice Int                     deriving (Show, Eq)
-- data Command    = Movement Move | Menu | Quit                  deriving (Show, Eq)

-- end of paste from logic

-- possible actions from Logic

-- data TurnAction = HeroMove Direction | Ranged Pos | Rest    deriving (Show,Eq)
data Action = TA TurnAction | Menu | Quit   | NoAction           deriving (Show,Eq)
data Choice = Accept | Deny | Choice Int    | NoChoice            deriving (Show,Eq)

yesNoChoice     = [Accept, Deny]
numeralChoice   = [Choice n | n <- [0..9]]
allChoices      = yesNoChoice ++ numeralChoice

type Message    = String
type Menu       = Message

parseCommand :: Char -> Action
parseCommand 'w' = TA $ HeroMove NORTH
parseCommand 's' = TA $ HeroMove SOUTH
parseCommand 'a' = TA $ HeroMove WEST
parseCommand 'd' = TA $ HeroMove EAST
parseCommand 'r' = TA $ HeroMove STAY
parseCommand '>' = TA $ ClimbDown
parseCommand '<' = TA $ ClimbUp
--for stupid dvorak users
parseCommand 't' = TA $ HeroMove NORTH
parseCommand 'n' = TA $ HeroMove SOUTH
parseCommand 'h' = TA $ HeroMove WEST
parseCommand '-' = TA $ HeroMove EAST

parseCommand 'm' = Menu
parseCommand 'q' = Quit
parseCommand  _  = NoAction

--possible alternative
--parseChoice :: Char -> [Choice] -> Maybe Choice
--which would do the checking for valid choice

parseChoice :: Char -> Choice
parseChoice 'y' = Accept
parseChoice 'n' = Deny
parseChoice  c  | c `elem` ['0'..'9']   = Choice (digitToInt c)
                | otherwise             = NoChoice

--abstraction for taking commands, run with parse* to get an action. Daniel
--Would need another layer over Action to return Actions and Choices at the same time
--is it worth the mess? Nic

readInput :: (Char -> a) -> IO (a)
readInput rf = do
    c <- getChar
    return (rf c)

readCommand = readInput parseCommand
readChoice  = readInput parseChoice

ask :: Menu -> [Choice] -> IO(Choice)
ask m cs = do
    drawMenu m
    e <- getChar
    let choice = parseChoice e in
        case choice of
            NoChoice    -> ask m cs
            otherwise   -> if choice `elem` cs then return choice else ask m cs
