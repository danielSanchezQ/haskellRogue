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
import Data.List(find,delete)


data TurnAction = HeroMove Direction | Ranged Pos | Rest    deriving (Show,Eq)

newGame :: GameState
newGame = GameState {hero=newHero, entities=[], world=standardMap 0}

newHero :: Hero
newHero = Entity {ename="Urist", elifes=3, ejob=NoJob, eweapon=NoWeapon, eposition=(9,5), erace=Hero, ebehav=Seek}

--Does not check entity position
addEnt :: GameState -> Entity -> GameState
addEnt gameState ent = gameState {entities=entities gameState ++[ent]}

moveHero :: GameState -> Direction -> Maybe GameState
moveHero gameState dir
    | isPositionWalkable gameState (x2,y2)  = Just gameState {hero = moveEntity (hero gameState) dir}
    | isPositionAttackable gameState (x2,y2)= Just $ doCombat gameState (x2,y2)
    | otherwise                             = Nothing
        where
            (x1,y1) = getPosition $ getHero gameState
            (x2,y2) = makeMove' (x1,y1) dir

doCombat :: GameState -> Pos -> GameState
doCombat gameState pos = gameState {hero=newHero, entities=newEntities}
    where
        (newHero,newEnt) = attack (hero gameState) enemy
        enemy :: Entity
        enemy = case lookupPos pos $ getEnts gameState of
                    Just e  -> e
                    Nothing -> error "attacked an empty position"
        newEntities = if (getHealth newEnt) > 0 then replaceEnt newEnt $ getEnts gameState
                        -- else delete enemy $ getEnts gameState
                        else addRandomEnemy $ delete enemy $ getEnts gameState

addRandomEnemy :: [Entity] -> [Entity]
addRandomEnemy = (++ [randomEnt])

replaceEnt :: Entity -> [Entity] -> [Entity]
replaceEnt e [] = [e]
replaceEnt e (e1:es)
    | getPosition e == getPosition e1     = e:es
    | otherwise                 = e1: replaceEnt e es

lookupPos :: Pos -> [Entity] -> Maybe Entity
lookupPos pos = find ((==) pos . getPosition)

isPositionWalkable :: GameState -> Pos -> Bool
isPositionWalkable gs pos = isPositionValid gs pos &&
                            isPositionEmpty gs pos &&
                            ((getCell (getMap gs) pos) == Empty)

isPositionAttackable :: GameState -> Pos -> Bool
isPositionAttackable gs pos = isPositionValid gs pos &&
                              (Nothing /= (lookupPos pos $ getEnts gs))

-- | checks if there are entities on a position
isPositionEmpty :: GameState -> Pos -> Bool
isPositionEmpty gs pos = isPositionValid gs pos && (not $ any ((pos ==) . getPosition) $ entities gs)

isPositionValid :: GameState -> Pos -> Bool
isPositionValid gameState pos = isValidPos pos
    where
        isValidPos (x,y) = (x>0) && (y>0) && (x<=xmax) && (y<=ymax)
        (xmax, ymax) = getSize $ getMap gameState

healHero :: GameState -> GameState
healHero = id

attackEntity :: GameState -> Direction -> GameState
attackEntity = const

step :: TurnAction -> GameState -> GameState
step (Rest) gs = gs
step (Ranged _) gs = gs
step (HeroMove dir) gs = 
        case (newGS) of
            Nothing         -> gs
            Just ngs        -> ngs
        where
            newGS = moveHero gs dir
