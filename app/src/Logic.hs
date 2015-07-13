module Logic where

import Utils
import Entities
import Maps

data GameState = GameState { hero :: Hero,
                             entities :: [Entity],
                             world :: Floor
                           } deriving Show

data Action = Move

-- ToDo: pass random to map generator
newGame :: GameState
newGame = GameState {hero=newHero, entities=[], world=standardMap 0}

getMap :: GameState -> Floor
getMap = world

getHero :: GameState -> Hero
getHero = hero

getEnts :: GameState -> [Entity]
getEnts = entities

newHero :: Hero
newHero = Entity {ename="Urist", elifes=1, ejob=NoJob, eweapon=NoWeapon, eposition=(3,5), erace=Hero}

--Does not check entity position
addEnt :: GameState -> Entity -> GameState
addEnt gameState ent = gameState {entities=entities gameState ++[ent]}

moveHero :: GameState -> Pos -> Maybe GameState
moveHero gameState pos@(x2,y2)
    | isPositionValid gameState (x1 + x2, y1 + y2)     = Just newState
    | otherwise                             = Nothing
        where
            (x1,y1) = getPosition $ getHero gameState
            newState = gameState {hero = moveEntityP (hero gameState) pos}

isPositionValid :: GameState -> Pos -> Bool
isPositionValid gameState pos = isFloor && noEntity && (isValidPos pos)
    where
        isValidPos (x,y) = (x>0) && (y>0) && (x<=xmax) && (y<=ymax)
        (xmax, ymax) = getSize $ getMap gameState
        isFloor = (getCell (getMap gameState) pos) == Empty
        noEntity = not $ any ((pos ==) . getPosition) $ entities gameState

act :: GameState -> Action -> GameState
act = undefined
