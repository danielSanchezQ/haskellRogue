--AI module, will deal with the npc behaviours
module AI where

import Entities
import Utils


evalBehaviour :: Entity -> Hero -> Move
evalBehaviour e h = manageBehaviour (ebehav e) e h

manageBehaviour :: Behaviour -> Entity -> Hero -> Move
manageBehaviour bh e h = case bh of Seek        -> manageSeek   e h
                                    Escape      -> manageScape  e h
                                    Watch       -> manageWatch  e h
                                    NoBehave    -> STAY

manageSeek, manageScape, manageWatch :: Entity -> Hero -> Move
manageSeek  e h = movefromPos (eposition e) (eposition h)
manageScape e h = reverseMove $ movefromPos (eposition e) (eposition h)
manageWatch e h = STAY
