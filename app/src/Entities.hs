--HASKELL ROGUE GAME MODULE ENTITIES--
module Entities where
import Utils (Pos, movePos, up, down, left, right)

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
pickWeapon (Entity hname hlifes hjob hweapon pos) w  = Entity hname hlifes hjob w pos

changeJob :: Hero -> Job -> Entity
changeJob (Entity hname hlifes hjob hweapon pos) j  = Entity hname hlifes j hweapon pos

kill :: Entity -> Entity
kill (Entity hname hlifes hjob hweapon pos)         = Entity hname 0 hjob hweapon pos

hit :: Entity -> Int -> Entity
hit (Entity hname hlifes hjob hweapon pos) n        = Entity hname remained hjob hweapon pos
    where
        remained = hlifes - n

getPosition :: Entity -> Pos
getPosition = eposition

exampleEntity :: Entity
exampleEntity = Entity [] 1 Mage exampleWeapon (2,3)

exampleWeapon :: Weapon
exampleWeapon = Weapon "example weapon" 1 Rod
