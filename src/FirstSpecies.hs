module FirstSpecies
( firstSpeciesGenerator
) where

import BlackboardSystem.KnowledgeSource
import BlackboardSystem.Blackboard
import Music.Note

-- generate a random whole note. TODO figure out the limits on the randomness of pitch.
firstSpeciesGenerator = makeGenerator op --KS { isTester = False, isSoftRule = False, operate = op }
  where op = undefined
