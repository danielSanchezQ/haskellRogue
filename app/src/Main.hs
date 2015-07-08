import Maps
import UI
import Entities
import Utils

main :: IO()
main = do
    putStrLn $ unlines $ drawMap exampleMap
