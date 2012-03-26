module FirstSpecies
( agents
) where

import BlackboardSystem.KnowledgeSource
import BlackboardSystem.Blackboard
import Music.Note
import Music.Voice
import Util.Zipper
import Music.Scale
import Music.Interval
import Music.Duration (whole)
import System.Random

agents :: [ KnowledgeSource ]
agents = [ firstSpeciesGenerator 
         , consonantDownbeats
         ]

safeHead (x:_) = Just x
safeHead [] = Nothing

{- Generate a whole note simultaneous with a note of the cantus firmus.
   Limit to one or zero accidentals. Limit to current note +/- 2 octaves.
   Everything in first species must be consonant, so just look at stuff in the scale.
   Place the generated note at the end of the counterpoint. -}
firstSpeciesGenerator = makeGenerator (\bb ->
  let cpTime = durationOfCounterPoint bb
      cfNote = goToTime' (cantusFirmus bb) cpTime
      isSimul = cpTime == startTimeOfFocus cfNote
      prevCPNote = back (end (counterPoint bb))
      base = pitch (head (maybe (focus cfNote) focus prevCPNote))
      scl = scale bb
      gen = randGen bb
      (scaleIdx, gen') = randomR (0, lenBS scl - 1) gen 
      (octaveOff, gen'') = randomR (False, True) gen' 
      (octaveUp, gen''') = randomR (False, True) gen'' 
      scalePitch = scl $# scaleIdx 
      offsetPitch | not octaveOff && octaveUp     = scalePitch
                  | octaveOff && octaveUp         = octave #^ scalePitch
                  | not octaveOff && not octaveUp = octave #. scalePitch
                  | octaveOff && not octaveUp     = (octave ## octave) #. scalePitch
      newNote = wrapWithDur offsetPitch whole
      newCP = modify (end (counterPoint bb)) (const [newNote])
      newBB = setGen (modifyCP bb (const newCP)) gen'''
  in if not isSimul then bb else newBB) 
  "First species generator"

consonantDownbeats = makeHardRule (\bb ->
  let cpNoteM = goToTime (counterPoint bb) (timeToTestAt bb) >>= safeHead . focus
      cfNoteM = goToTime (cantusFirmus bb) (timeToTestAt bb) >>= safeHead . focus
      intervalM = cpNoteM >>= \cp -> cfNoteM >>= \cf -> return (pitch cp # pitch cf)
      isConsonantM = intervalM >>= \i -> let ls = lspan (simplify i) in return (ls `elem` [1,3,5,6,8])
      result = maybe False id isConsonantM
  in  (if result then passTest else failTest) bb)
  "First species - all downbeats must be consonant (1st, 3rd, 5th, 6th, 8th & compounds)"
