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
activeTests =
            -- testState:
            testMove myGame:
            []

-- testDraw :: IO()
-- testDraw = putStrLn $ unlines $ draw standardMap --[exampleEntity] $ standardMap (0)
-- testState = putStrLn $ unlines $ draw (getHero myGame:getEnts myGame) (getMap myGame)

myGame = addEnt newGame exampleEntity

testMove :: GameState -> IO()
testMove gs = case moveHero gs RIGHT of
                Nothing -> putStrLn "Illegal Move"
                Just gs -> draw gs -- (getHero gs : (getEnts gs)) $ getMap gs

