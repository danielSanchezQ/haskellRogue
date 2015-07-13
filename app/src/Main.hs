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
            (Act Quit)  -> do   putStrLn "Goodbye"
                                return ()
            (Act a)     -> do   print command
                                -- gameLoop $ step a gameState

myGame = addEnt newGame exampleEntity

main :: IO()
main = do
        gameLoop myGame
