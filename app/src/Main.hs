import Maps
import UI
import Entities
import Utils
import Graphics
import Logic
import System.IO

gameLoop :: GameState -> IO()
gameLoop gameState = do
        if (getHealth $ getHero $ gameState) <= 0 then do
            choice <- ask "/nYou died!!/nWant to try again y/n" yesNoChoice
            case choice of
                Accept      -> gameLoop myGame
                Deny        -> putStrLn "Goodbye" >> return ()
        else do
            putStrLn "---------------------\n"
            draw gameState
            command <- readCommand
            -- print command
            case command of
                (NoAction)  -> gameLoop gameState
                (Quit)  -> do   putStrLn "Goodbye"
                                return ()
                (TA a)     -> gameLoop $ step a gameState
                otherwise   -> do print $ "Unexpected command:" ++ (show command)
                                  gameLoop gameState

myGame = addEnt newGame exampleEntity

main :: IO()
main = do
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        gameLoop myGame
