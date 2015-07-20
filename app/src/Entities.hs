{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
--HASKELL ROGUE GAME MODULE ENTITIES--
module Entities  where
import Utils (Pos, Move(..), makeMove', movePos)
import Maps
import System.Random(RandomGen, randomR, Random, random)

data Job        =  Mage     | Healer| Assassin  | Barbarian | NoJob                        deriving (Show,Eq,Enum,Bounded)
data WType      =  Sword    | Bow   | Rod       | Magic                                    deriving (Show,Eq,Enum,Bounded)
data Race       =  Hero     | Human | Troll     | Dragon | Elven | Feline | Dwarven        deriving (Show,Eq,Enum,Bounded)
data Behaviour  =  Seek     | Escape| Watch     | NoBehave                                 deriving (Show,Eq)

data Weapon =  Weapon { wname   :: String,
                        wpower  :: Int,
                        wtype   :: WType} | NoWeapon                deriving (Show,Eq)

data Entity =  Entity { ename    :: String,
                        elives   :: Int,
                        ejob     :: Job,
                        eweapon  :: Weapon,
                        eposition:: Pos,
                        erace    :: Race,
                        ebehav   :: Behaviour }                     deriving (Show,Eq)


type Hero       = Entity
type Monster    = Entity


instance (Enum a, Bounded a) => Random a where
    randomR (a,b) g = case (randomR (fromEnum a, fromEnum b) g) of
        (x, g') -> (toEnum x, g')
    random g = randomR (minBound,maxBound) g

instance Random Entity where
    randomR _ = random
    random g = (Entity "Random entity" health job weapon (0,0) race Seek,lastG)
        where
            (health, g1) = randomR (1,5) g
            (job, g2) = random g1
            (weapon, g3) = random g2
            (race, lastG) = randomR (Human,Dwarven) g3

instance Random Weapon where
    randomR _ = random
    random g = (Weapon "Random weapon" power wtype, lastG)
        where
            (power, g1) = randomR (1,5) g
            (wtype, lastG) = random g1

attack :: Entity -> Entity -> (Entity, Entity)
attack hero monster = (hero{elives = elives hero -1}, monster{elives = elives monster -1})

pickWeapon :: Hero -> Weapon -> Entity
pickWeapon e w = e {eweapon = w}

changeJob :: Hero -> Job -> Entity
changeJob e j  = e {ejob = j}

kill :: Entity -> Entity
kill e = e {elives = 0}

hit :: Entity -> Int -> Entity
hit e n = e {elives = remained}
    where
        remained = elives e - n

moveEntity :: Entity -> Move -> Entity
moveEntity e m = e {eposition = makeMove' (eposition e) m}

positionEntity :: Entity -> Pos -> Entity
positionEntity e pos = e {eposition = pos}

-- not needed ATM
-- moveEntityP :: Entity -> Pos -> Entity
-- moveEntityP e pos = e {eposition = movePos pos (eposition e)}

getRace :: Entity -> Race
getRace = erace

getPosition :: Entity -> Pos
getPosition = eposition

getHealth :: Entity -> Int
getHealth = elives

getName :: Entity -> String
getName = ename

getJob :: Entity -> Job
getJob = ejob

exampleEntity :: Entity
exampleEntity = Entity [] 1 Mage exampleWeapon (2,3) Elven Seek

exampleWeapon :: Weapon
exampleWeapon = Weapon "example weapon" 1 Rod
