-------------------------------------------------------------------------------
-- |
-- Module       : Logic
--
-- This module contains all the data structures and functions related to the
-- game logics: handling of game states, applying actions to game states and
-- calculating allowed actions.
-- This module does not need to know about IO(), Graphics, generation of maps.
-- It should export: GameState (without constructors), possible actions to take
-- and ways of applying them to the game state, ways to change the game state
-- over the passing of a turn (or a set amount of time), functions to access
-- the Floor and the Entities.
-- It shoud not expose the inner workings of GameState and turn p
--
-- ToDo:
-- - decide if invalid game state changes should return Maybe GameState or an
--   unchanged game state
-------------------------------------------------------------------------------

module Logic
    (
        GameState(),
        TurnAction(..),
        newGame,
        getMap,
        getHero,
        getEnts,
        newHero,
        addEnt,
        step
        )
        where

import Utils
import Entities
import Maps

data TurnAction = HeroMove Direction | Ranged Pos | Rest    deriving (Show,Eq)

data GameState = GameState { hero :: Hero,
                             entities :: [Entity],
                             world :: Floor
                           } deriving Show

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

moveHero :: GameState -> Direction -> Maybe GameState
moveHero gameState dir
    | isPositionValid gameState (x1 + x2, y1 + y2)     = Just newState
    | otherwise                             = Nothing
        where
            (x1,y1) = getPosition $ getHero gameState
            (x2,y2) = makeMove' (x1,y1) dir 
            newState = gameState {hero = moveEntity (hero gameState) dir}

isPositionValid :: GameState -> Pos -> Bool
isPositionValid gameState pos = isFloor && noEntity && (isValidPos pos)
    where
        isValidPos (x,y) = (x>0) && (y>0) && (x<=xmax) && (y<=ymax)
        (xmax, ymax) = getSize $ getMap gameState
        isFloor = (getCell (getMap gameState) pos) == Empty
        noEntity = not $ any ((pos ==) . getPosition) $ entities gameState

healHero :: GameState -> GameState
healHero = id

step :: TurnAction -> GameState -> GameState
step (Rest) gs = gs
step (Ranged _) gs = gs
step (HeroMove dir) gs = 
        case (newGS) of
            Nothing         -> gs
            Just ngs        -> ngs
        where
            newGS = moveHero gs dir
