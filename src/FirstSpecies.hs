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

type Time = Double

agents :: [Agent]
agents = [
         ]

fromMaybe def may = maybe def id may
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
firstSpeciesGenerator = makeGenerator (\bb ->
  let lastCPVoice = back1 (end (counterPoint bb))
      lastCPNote = getCurrentNote =<< lastCPVoice
      cpEndTime = durationOfCounterPoint bb
      corrCFVoice = goToTime (cantusFirmus bb) cpEndTime
      corrCFNote = getCurrentNote =<< corrCFVoice
      isSimul = toBool (liftM ((==cpEndTime) . startTimeOfFocus) lastCPVoice)
      -- random note generation
      unsafeLastCP = fromJust lastCPNote
      (letIdx, gen') = randomR (0, fromEnum (maxBound :: Letter)) (rGen bb)
      (newOct, gen'') = let o = octave unsafeLastCP in randomR (o-2,o+1)  gen'
      (newAcc, gen''') = randomR (negate 1, 1) gen''
      newNote = makeNote (toEnum letIdx) newOct newAcc (halves whole) (dots whole)
      newCP = modify (const [newNote]) (end (counterPoint bb)) 
      newBB = setGen (modifyCP bb (const newCP)) gen'''
  in if isSimul then newBB else bb)
  "First species generator"

consonantDownbeats = makeHardRule (\bb ->
  let cpNote = goToTime (counterPoint bb) (timeToTestAt bb) >>= getCurrentNote
      cfNote = goToTime (cantusFirmus bb) (timeToTestAt bb) >>= getCurrentNote
      interval = liftM2 (#) cpNote cfNote
      isConsonant = (`elem`[unison,min3,maj3,perf5,min6,maj6,octv]) . simplify
      result = toBool $ liftM isConsonant interval
  in if result || not (isFirstSpecies bb (timeToTestAt bb)) then passTest bb else failTest bb)
  "First species - all downbeats must be consonant (1st, 3rd, 5th, 6th, 8th & compounds)"
