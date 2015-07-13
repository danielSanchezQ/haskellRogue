import Maps
import UI
import Entities
import Utils
import Graphics
import Logic

gameLoop :: GameState -> IO()
gameLoop gameState = do
        putStrLn "---------------------\n"
        draw gameState
        command <- readCommand
        print command
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
        gameLoop myGame
