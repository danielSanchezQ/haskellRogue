--HASKELL ROGUE GAME MODULE ENTITIES--


data Job    =  Mage | Healer | Assassin | Barbarian     deriving (Show)
data WType  =  Sword | Bow | Rod | Magic                deriving (Show)

data Weapon =  Weapon { wname   :: String, 
                        wpower  :: Int, 
                        wtype   :: WType}          deriving (Show)

data Entity =  Entity { ename    :: String,
                        elifes   :: Int,
                        ejob     :: Job, 
                        eweapon  :: Weapon,
                        eposition:: (Int, Int)}    deriving (Show)


type Hero       = Entity
type Monster    = Entity




attack :: Entity -> Entity -> (Entity, Entity)
attack (Hero hname hlifes hjob hweapon _)  (Monster mname mlifes mjob mweapon _) = ()