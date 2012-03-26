module FirstSpecies
( firstSpeciesGenerator
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
      (scaleIdx, gen') = randomR (0, lenS scl) gen 
      (octaveOff, gen'') = randomR (False, True) gen' 
      (octaveUp, gen''') = randomR (False, True) gen'' 
      scalePitch = getNote scl base scaleIdx
      offsetPitch | not octaveOff && octaveUp     = scalePitch
                  | octaveOff && octaveUp         = octave #^ scalePitch
                  | not octaveOff && not octaveUp = octave #. scalePitch
                  | octaveOff && not octaveUp     = (octave ## octave) #. scalePitch
      newNote = wrapWithDur offsetPitch whole
      newCP = modify (end (counterPoint bb)) (const [newNote])
      newBB = setGen (modifyCP bb (const newCP)) gen'''
  in if not isSimul then bb else newBB) 
