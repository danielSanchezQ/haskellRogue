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
        stepHero,
        stepEntities,
        step
        )
        where

import Utils
import Entities
import Maps
import Data.List(find,delete)
import AI
import System.Random(RandomGen, randomR,split)

data TurnAction = HeroMove Direction | Ranged Pos | Rest    deriving (Show,Eq)

newGame :: RandomGen g => g -> GameState
newGame g = GameState {hero=newHero, entities=[], world=standardMap g}

newHero :: Hero
newHero = Entity {ename="Urist", elives=10, ejob=NoJob, eweapon=NoWeapon, eposition=(5,5), erace=Hero, ebehav=Seek}

placeRandomEnt :: RandomGen g => g -> GameState -> GameState
placeRandomEnt ranGen gameState = addEnt (positionEntity randomEnt validPos) gameState
    where
        validPos = findRandomSpot ran1 gameState
        randomEnt = randomEntity ran2
        (ran1,ran2) = split ranGen

findRandomSpot :: RandomGen g => g -> GameState -> Pos
findRandomSpot _ _ = (15,5)

--Does not check entity position
addEnt :: Entity -> GameState -> GameState
addEnt ent gameState = gameState {entities=entities gameState ++[ent]}

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
                        else delete enemy $ getEnts gameState

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
                            (cell == Empty || cell == Door)
                        where
                            cell = getCell (getMap gs) pos

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
healHero gs = gs {hero = (getHero gs) {elives = elives (getHero gs) + 1}}

attackEntity :: GameState -> Direction -> GameState
attackEntity = const

stepHero :: TurnAction -> GameState -> GameState
stepHero (HeroMove STAY) gs = healHero gs
stepHero (Ranged _) gs = gs
stepHero (HeroMove dir) gs = 
        case (newGS) of
            Nothing         -> gs
            Just ngs        -> ngs
        where
            newGS = moveHero gs dir

addNewEnemies :: RandomGen g => g -> GameState -> GameState
addNewEnemies ranGen gs
    | length (getEnts gs) > 3  = gs
    | otherwise             = addNewEnemies ran2 $ placeRandomEnt ran1 gs
        where
            (ran1,ran2) = split ranGen


stepEntities :: RandomGen g => g -> GameState -> GameState
stepEntities ranGen gs = addNewEnemies ranGen $ gs {entities = map (\x-> let m = evalBehaviour x gs in (if isPositionWalkable gs (makeMove' (eposition x) m)  then moveEntity x m else x)) (entities gs)}

step :: RandomGen g => g -> TurnAction -> GameState -> GameState
step ranGen ta gs = stepEntities ranGen $ stepHero ta gs
