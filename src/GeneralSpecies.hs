module GeneralSpecies
( beginPerfectConsonance
) where

import BlackboardSystem.KnowledgeSource
import BlackboardSystem.Blackboard
import Util.Zipper
import Music.Interval
import Music.Note

-- A general counterpoint hard rule: the first notes must be in a perfect consonance.
beginPerfectConsonance = KS { isTester = True, isSoftRule = False, operate = op }
  where op bb = let (cfStart:_) = escape (cantusFirmus bb)
                    (cpStart:_) = escape (counterPoint bb)
                in bb { testResult = quality (pitch cfStart # pitch cpStart) == Perfect }
