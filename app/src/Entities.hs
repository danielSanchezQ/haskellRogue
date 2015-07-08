--HASKELL ROGUE GAME MODULE ENTITIES--
module Entities where
import Utils (Pos, movePos, up, down, left, right)

data Job    =  Mage | Healer | Assassin | Barbarian     deriving (Show)
data WType  =  Sword | Bow | Rod | Magic                deriving (Show)

data Weapon =  Weapon { wname   :: String, 
                        wpower  :: Int, 
                        wtype   :: WType}          deriving (Show)

data Entity =  Entity { ename    :: String,
                        elifes   :: Int,
                        ejob     :: Job, 
                        eweapon  :: Weapon,
                        eposition:: Pos}    deriving (Show)


type Hero       = Entity
type Monster    = Entity




attack :: Entity -> Entity -> (Entity, Entity)
attack hero@(Entity hname hlifes hjob hweapon _)  monster@(Entity mname mlifes mjob mweapon _) = (hero , monster)