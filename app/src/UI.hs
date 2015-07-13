module UI where
import Control.Monad
import Utils        (Move(..))
import Graphics     (drawMenu)
import Data.Char    (digitToInt)
import Logic


yesNoChoice     = [Accept, Deny]
numeralChoice   = [Choice n | n <- [0..9]]
allChoices      = yesNoChoice ++ numeralChoice

type Message    = String
type Menu       = Message

parseCommand :: Char -> Action Command
parseCommand 'w' = Act  (Movement UP    )
parseCommand 's' = Act  (Movement DOWN  )
parseCommand 'a' = Act  (Movement LEFT  )
parseCommand 'd' = Act  (Movement RIGHT )
parseCommand 'm' = Act  Menu
parseCommand 'q' = Act  Quit
parseCommand  _  = None

parseChoice :: Char -> Action Choice
parseChoice 'y' = Act Accept
parseChoice 'n' = Act Deny
parseChoice  c  | c `elem` ['0'..'9']   = Act (Choice (digitToInt c))
                | otherwise             = None

--abstraction for taking commands, run with parse* to get an action. Daniel
readInput :: (Char -> Action a) -> IO (Action a)
readInput rf = do
    c <- getChar
    return (rf c)

readCommand = readInput parseCommand
readChoice  = readInput parseChoice

ask :: Eq a => Menu -> [a] -> (Menu -> IO(Action a)) -> IO(Action a)
ask m cs rf = do
    drawMenu m
    e <- rf m
    case e of
        None        -> ask m cs rf
        otherwise   -> if e `elem` [Act x | x <- cs] then return e else ask m cs rf

----ask "asndfoasidhfaspodifh" [Accept, Deny] readChoice

