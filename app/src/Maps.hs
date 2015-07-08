module Maps where

data Cell = W | F | D
    deriving (Show, Read)

type Map = [[Cell]]

exampleMap :: Map
exampleMap = map read ["WWWWW", "WFFFW", "WFFFW","WWWWW"]

drawMap :: Map -> String
drawMap = show
