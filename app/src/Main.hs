import Maps
import UI
import Entities
import Utils
import Graphics
import Logic

gameLoop :: GameState -> IO()
gameLoop gameState = do
        draw gameState
        command <- readCommand
        case command of
            Quit -> do putStrLn "Goodbye"
                       return ()
            otherwise -> do print command
                            gameLoop $ step command gameState

myGame = addEnt newGame exampleEntity

main :: IO()
main = do
        gameLoop myGame
