import Maps
import UI
import Entities
import Utils
import Graphics

main :: IO()
main = do
    putStrLn $ unlines $ drawMap exampleMap
