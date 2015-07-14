--HASKELL ROGUE GAME MODULE ENTITIES--
module Entities  where
import Utils (Pos, Move(..), makeMove', movePos)

data Job    =  Mage | Healer | Assassin | Barbarian | NoJob         deriving (Show,Eq)
data WType  =  Sword | Bow | Rod | Magic                            deriving (Show,Eq)
data Race   =  Hero | Human | Troll | Dragon | Elven | Feline | Dwarven    deriving (Show,Eq)

data Weapon =  Weapon { wname   :: String, 
                        wpower  :: Int, 
                        wtype   :: WType} | NoWeapon                deriving (Show,Eq)

data Entity =  Entity { ename    :: String,
                        elifes   :: Int,
                        ejob     :: Job, 
                        eweapon  :: Weapon,
                        eposition:: Pos,
                        erace    :: Race }                            deriving (Show,Eq)


type Hero       = Entity
type Monster    = Entity

attack :: Entity -> Entity -> (Entity, Entity)
attack hero monster = (hero{elifes = elifes hero -1}, monster{elifes = elifes monster -1})

pickWeapon :: Hero -> Weapon -> Entity
pickWeapon e w = e {eweapon = w}

changeJob :: Hero -> Job -> Entity
changeJob e j  = e {ejob = j}

kill :: Entity -> Entity
kill e = e {elifes = 0}

hit :: Entity -> Int -> Entity
hit e n = e {elifes = remained}
    where
        remained = elifes e - n

moveEntity :: Entity -> Move -> Entity
moveEntity e m = e {eposition = makeMove' (eposition e) m}

-- not needed ATM
-- moveEntityP :: Entity -> Pos -> Entity
-- moveEntityP e pos = e {eposition = movePos pos (eposition e)}

getRace :: Entity -> Race
getRace = erace

getPosition :: Entity -> Pos
getPosition = eposition

getHealth :: Entity -> Int
getHealth = elifes

exampleEntity :: Entity
exampleEntity = Entity [] 1 Mage exampleWeapon (2,3) Elven

randomEnt :: Entity
randomEnt = Entity [] 5 Healer exampleWeapon (12,7) Troll

exampleWeapon :: Weapon
exampleWeapon = Weapon "example weapon" 1 Rod
