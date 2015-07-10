import Maps
import UI
import Entities
import Utils
import Graphics
import IO
import Control.Monad(sequence_)


-- main :: IO(a->a)
main = do
    sequence_ activeTests

-- activeTests :: [IO()]
activeTests = testDraw:
              []

-- testDraw :: IO()
testDraw = putStrLn $ unlines $ draw [exampleEntity] $ standardMap (0)
