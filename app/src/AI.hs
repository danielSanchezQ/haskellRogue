--AI module, will deal with the npc behaviours

import Entities
import Utils


evalBehaviour :: Entity -> GameState -> Move
evalBehaviour e gs = 


manageBehaviour :: Behaviour -> Entity -> Hero -> Move
manageBehaviour bh e h = case bh of Seek        -> manageSeek   e h
                                    Escape      -> manageScape  e h
                                    Watch       -> manageWatch  e h
                                    NoBehave    -> STAY

manageSeek, manageScape, manageWatch :: Entity -> Hero -> Move
manageSeek  e h = movefromPos epos e epos h
manageScape e h = reverseMove $ movefromPos epos e epos h
manageWatch e h = STAY