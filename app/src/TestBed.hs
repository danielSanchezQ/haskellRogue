import Maps
import UI
import Entities
import Utils
import Graphics
import Logic
import IO
import Control.Monad(sequence_)


-- main :: IO(a->a)
main = do
    sequence_ activeTests

-- activeTests :: [IO()]
activeTests = testState:
              testMove myGame:
              []

-- testDraw :: IO()
testDraw = putStrLn $ unlines $ draw [exampleEntity] $ standardMap (0)
testState = putStrLn $ unlines $ draw (getHero myGame:getEnts myGame) (getMap myGame)

myGame = addEnt newGame exampleEntity

testMove :: GameState -> IO()
testMove gs = case moveHero gs (-1,-1) of
                Nothing -> putStrLn "Illegal Move"
                Just gs -> putStrLn $ unlines $ drawGameState gs
