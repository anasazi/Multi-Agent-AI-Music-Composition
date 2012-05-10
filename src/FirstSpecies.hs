module FirstSpecies (agents) where

import Agent
import Blackboard

import Note
import Interval
import Voice
import Scale

import System.Random
import Control.Monad
import Data.Function (on)
import Data.Maybe (fromMaybe)

type Time = Double

agents :: [Agent]
agents = [ firstSpeciesGenerator
         , consonantDownbeats
         , approachPerfIntervalsByContraryOrOblique 
         , noSimulRepeatsInBothCFAndCP 
         ]

toBool = fromMaybe False

fromJust (Just x) = x
fromJust Nothing = error "fromJust on Nothing"

{- Test if at a particular time we are in first species.
   That is, are there simultaneous whole notes in both the CF and CP -}
isFirstSpecies :: Blackboard -> Time -> Bool
isFirstSpecies bb t =
  let cpVoice = goToTime (counterPoint bb) t
      cfVoice = goToTime (cantusFirmus bb) t
      cpNote = getCurrentNote =<< cpVoice
      cfNote = getCurrentNote =<< cfVoice
      isWholeM = liftM $ on (==) Duration whole
  in all (toBool . isWholeM) [ cpNote, cfNote ]
     && toBool (liftM2 (on (==) startTimeOfFocus) cpVoice cfVoice)

{- Generate a whole note simultaneous with a note of the CF.
   Limit to one or zero accidentals.
   Limit to current note +/- 2 octaves.
   Place generated note at the end of the counterpoint. -}
firstSpeciesGenerator = makeGenerator op "First species generator"
  where
  op bb = 
    let lastCPVoice = end (counterPoint bb)
        lastCPNote = getCurrentNote =<< back1 lastCPVoice
        cpEndTime = durationOfCounterPoint bb
        corrCFVoice = goToTime (cantusFirmus bb) cpEndTime
        corrCFNote = getCurrentNote =<< corrCFVoice
        isSimul = startTimeOfFocus lastCPVoice == cpEndTime
        -- random note generation
        baseNote = fromMaybe (fromJust corrCFNote) lastCPNote
        (letIdx, gen') = randomR (0, fromEnum (maxBound :: Letter)) (rGen bb)
        (newOct, gen'') = let o = octave baseNote in randomR (o-1,o)  gen'
        (newAcc, gen''') = randomR (negate 1, 1) gen''
        newNote = makeNote (toEnum letIdx) newOct newAcc (halves whole) (dots whole)
        newCP = modify (const [newNote]) (end (counterPoint bb)) 
        newBB = setGen (modifyCP bb (const newCP)) gen'''
    in if isSimul then newBB else bb

-- hard rule 1
consonantDownbeats = makeHardRule op 
  "First species - all downbeats must be consonant (1st, 3rd, 5th, 6th, 8th & compounds)"
  where
  op bb =
    let cpNote = goToTime (counterPoint bb) (timeToTestAt bb) >>= getCurrentNote
        cfNote = goToTime (cantusFirmus bb) (timeToTestAt bb) >>= getCurrentNote
        interval = liftM2 (#) cpNote cfNote
        isConsonant = (`elem`[unison,min3,maj3,perf5,min6,maj6,octv]) . simplify
        result = toBool $ liftM isConsonant interval
    in if not (isFirstSpecies bb (timeToTestAt bb)) || result then passTest bb else failTest bb

-- hard rule 2
approachPerfIntervalsByContraryOrOblique = flip makeHardRule "First species - Movement to a perfect interval must be contrary or oblique." $ \bb->
  let cf = goToTime (cantusFirmus bb) (timeToTestAt bb)
      cp = goToTime (counterPoint bb) (timeToTestAt bb)
      cfNotes = cf >>= getBackN 2
      cpNotes = cp >>= getBackN 2
      cfUp = toBool $! cfNotes >>= \[cur,bk1] -> return $! Location bk1 < Location cur
      cpUp = toBool $! cpNotes >>= \[cur,bk1] -> return $! Location bk1 < Location cur
      isContraryOrOblique = (cfUp || cpUp) && not (cfUp && cpUp)
      isToPerfect = toBool $! cfNotes >>= \[cf,_] -> cpNotes >>= \[cp,_] -> return (quality (cf # cp) == Perfect)
  in if not (isFirstSpecies bb (timeToTestAt bb)) || not isToPerfect || isContraryOrOblique then passTest bb else failTest bb

-- hard rule 3
noSimulRepeatsInBothCFAndCP = flip makeHardRule "First species - Notes may be repeated in either CF or CP but not both." $ \bb ->
  let cf = goToTime (cantusFirmus bb) (timeToTestAt bb)
      cp = goToTime (counterPoint bb) (timeToTestAt bb)
      cfNotes = cf >>= getBackN 2
      cpNotes = cp >>= getBackN 2
      bothRepeat = toBool $! cfNotes >>= \[cf1,cf2] -> cpNotes >>= \[cp1,cp2] -> return $! cf1 == cf2 && cp1 == cp2
  in if not (isFirstSpecies bb (timeToTestAt bb)) || not bothRepeat then passTest bb else failTest bb

-- hard rule 4
max4NoteParallelRun = flip makeHardRule "First species - CP and CF may run parallel for up to four notes max." $ \bb ->
  undefined
-- hard rule 5
skipsLessThanHalf = flip makeHardRule "First species - Skips must be less than half of melodic motions." $ \bb ->
  undefined
-- hard rule 6
noDirectRepetition = undefined
only2SequentialRepetition = undefined
-- soft rule 1
avoidMoreThan2PerfVertAdjacent = undefined
-- soft rule 2
avoidUnisonExceptStartAndEnd = undefined
-- soft rule 3
avoidSimulSkipsUnlessSmall = undefined
-- soft rule 4
avoidWideSeparation = undefined
-- soft rule 5
preferDirChangeAfterLargeSkipAndFollowWithStep = undefined
-- soft rule 6
tryToConnectExtremesWithSteps = undefined
-- soft rule 7
coverWholeOctaveEvery10To20Notes = undefined
