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

-- TODO
approachPerfIntervalsByContraryOrOblique = undefined
noSimulRepeatsInBothCFAndCP = undefined
max4NoteParallelRun = undefined
skipsLessThanHalf = undefined
noDirectRepetition = undefined
only2SequentialRepetition = undefined
avoidMoreThan2PerfVertAdjacent = undefined
avoidUnisonExceptStartAndEnd = undefined
avoidSimulSkipsUnlessSmall = undefined
avoidWideSeparation = undefined
preferDirChangeAfterLargeSkipAndFollowWithStep = undefined
tryToConnectExtremesWithSteps = undefined
coverWholeOctaveEvery10To20Notes = undefined
