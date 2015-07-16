import Maps
import UI
import Entities
import Utils
import Graphics
import Logic
import System.IO
import System.Random (getStdGen,RandomGen)



--using ranGen wrong
gameLoop :: RandomGen g => (GameState, g) -> IO()
gameLoop (gameState, ranGen) = do
        if (getHealth $ getHero $ gameState) <= 0 then do
            choice <- ask "\nYou died!!\nWant to try again y/n" yesNoChoice
            case choice of
                Accept      -> gameLoop (myGame ranGen, ranGen)
                Deny        -> putStrLn "Goodbye" >> return ()
        else do
            putStrLn "---------------------\n"
            clearAndDraw draw gameState
            command <- readCommand
            -- print command
            case command of
                (NoAction)  -> gameLoop (gameState, ranGen)
                (Quit)      -> do   putStrLn "Goodbye"
                                    return ()
                (TA a)      -> gameLoop (step a gameState, ranGen)
                otherwise   -> do print $ "Unexpected command:" ++ (show command)
                                  gameLoop (gameState, ranGen)

myGame :: RandomGen g => g -> GameState
myGame g = addEnt (newGame g) exampleEntity

main :: IO()
main = do
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        ranGen <- getStdGen
        gameLoop (myGame ranGen,ranGen)
