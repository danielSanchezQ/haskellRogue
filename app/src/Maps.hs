module Maps where

data Cell = W | F | D
    deriving (Show, Read)

type Map = [[Cell]]

exampleMap :: Map
exampleMap = [[W,W,W,W,W],[W,F,F,F,W],[W,F,F,F,W],[W,W,W,W,W]]

