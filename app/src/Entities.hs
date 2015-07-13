--HASKELL ROGUE GAME MODULE ENTITIES--
module Entities  where
import Utils (Pos, Move(..), makeMove', movePos)

data Job    =  Mage | Healer | Assassin | Barbarian | NoJob         deriving (Show)
data WType  =  Sword | Bow | Rod | Magic                            deriving (Show)
data Race   =  Hero | Human | Troll | Dragon | Elven | Feline | Dwarven    deriving (Show,Eq)

data Weapon =  Weapon { wname   :: String, 
                        wpower  :: Int, 
                        wtype   :: WType} | NoWeapon                deriving (Show)

data Entity =  Entity { ename    :: String,
                        elifes   :: Int,
                        ejob     :: Job, 
                        eweapon  :: Weapon,
                        eposition:: Pos,
                        erace    :: Race }                            deriving (Show)


type Hero       = Entity
type Monster    = Entity

attack :: Entity -> Entity -> (Entity, Entity)
attack hero@(Entity hname hlifes hjob hweapon _ _)  monster@(Entity mname mlifes mjob mweapon _ _) = undefined

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

exampleEntity :: Entity
exampleEntity = Entity [] 1 Mage exampleWeapon (2,3) Elven

exampleWeapon :: Weapon
exampleWeapon = Weapon "example weapon" 1 Rod
