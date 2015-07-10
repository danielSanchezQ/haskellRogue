--HASKELL ROGUE GAME MODULE ENTITIES--
module Entities  where
import Utils (Pos, Move(..), makeMove')

data Job    =  Mage | Healer | Assassin | Barbarian     deriving (Show)
data WType  =  Sword | Bow | Rod | Magic                deriving (Show)

data Weapon =  Weapon { wname   :: String, 
                        wpower  :: Int, 
                        wtype   :: WType}           deriving (Show)

data Entity =  Entity { ename    :: String,
                        elifes   :: Int,
                        ejob     :: Job, 
                        eweapon  :: Weapon,
                        eposition:: Pos}            deriving (Show)


type Hero       = Entity
type Monster    = Entity

attack :: Entity -> Entity -> (Entity, Entity)
attack hero@(Entity hname hlifes hjob hweapon _)  monster@(Entity mname mlifes mjob mweapon _) = undefined

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

getPosition :: Entity -> Pos
getPosition = eposition


exampleEntity :: Entity
exampleEntity = Entity [] 1 Mage exampleWeapon (2,3)

exampleWeapon :: Weapon
exampleWeapon = Weapon "example weapon" 1 Rod
