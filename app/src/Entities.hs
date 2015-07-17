--HASKELL ROGUE GAME MODULE ENTITIES--
module Entities  where
import Utils (Pos, Move(..), makeMove', movePos)
import Maps
import System.Random(RandomGen, randomR)

data Job        =  Mage     | Healer| Assassin  | Barbarian | NoJob                        deriving (Show,Eq)
data WType      =  Sword    | Bow   | Rod       | Magic                                    deriving (Show,Eq)
data Race       =  Hero     | Human | Troll     | Dragon | Elven | Feline | Dwarven        deriving (Show,Eq)
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

exampleEntity :: Entity
exampleEntity = Entity [] 1 Mage exampleWeapon (2,3) Elven Seek

randomEntity :: RandomGen g => g -> Entity
randomEntity ranGen = Entity [] health Healer exampleWeapon (0,0) Troll Seek
    where
        (health,_) = randomR (1,5) ranGen

exampleWeapon :: Weapon
exampleWeapon = Weapon "example weapon" 1 Rod
