module GeneralSpecies (agents) where

import Agent
import Blackboard

import Note
import Interval
import Voice

import Data.Function (on)
import Control.Monad
import Control.Applicative
import Control.Arrow
import Data.Maybe (fromMaybe)

agents :: [Agent]
agents = [ beginPerfectConsonance
         , noAugDimCPIntervals
         , dontSkipMoreThan6
         , noAug4Outline
         , endPerfectConsonance
         ]


toBool = fromMaybe False

beginPerfectConsonance = makeHardRule op "General CP - start with perfect consonance."
  where
  op bb = let cfStart = getCurrentNote . front . cantusFirmus $ bb
              cpStart = getCurrentNote . front . counterPoint $ bb
              interval = liftM2 (#) cfStart cpStart
              isPerfectConsonance i = quality i == Perfect && simplify i `elem` [unison, perf5, octv]
              noteOk = toBool . liftM isPerfectConsonance $ interval
              atFirstNote = timeToTestAt bb == 0
          in (if not atFirstNote || noteOk then passTest else failTest) bb

noAugDimCPIntervals = makeHardRule op "General CP - no augmented or diminished intervals in CP."
  where
  op bb = let cp = goToTime (counterPoint bb) (timeToTestAt bb)
              notes = getBackN 2 =<< cp
              interval = liftM2 (#) (liftM (!!0) notes) (liftM (!!1) notes)
              isDim = toBool . liftM ((<Minor) . quality) $ interval
              isAug = toBool . liftM ((>Major) . quality) $ interval
          in (if isDim || isAug then failTest else passTest) bb

dontSkipMoreThan6 = makeHardRule op "General CP - no skips larger than a sixth (except octave)."
  where
  op bb = let cp = goToTime (counterPoint bb) (timeToTestAt bb)
              notes = getBackN 2 =<< cp
              interval = liftM2 (#) (liftM (!!0) notes) (liftM (!!1) notes)
              tooBig = toBool . liftM ((> Span maj6) . Span) $ interval
              isOctv = toBool . liftM (== octv) $ interval
          in (if isOctv || not tooBig then passTest else failTest) bb

noAug4Outline = flip makeHardRule "General CP - no outlines (interval btw local extrema) of augmented fourths." $ \bb ->
    let cp = goToTime (counterPoint bb) (timeToTestAt bb)
        ext1 = cp >>= recentLocalExtreme
        ext2 = ext1 >>= recentLocalExtreme
        interval = liftM2 (#) (ext1 >>= getCurrentNote) (ext2 >>= getCurrentNote)
        isAug4 = toBool . liftM (==aug4) $ interval
    in (if isAug4 then failTest else passTest) bb

endPerfectConsonance = flip makeHardRule "General CP - end with perfect consonance." $ \bb ->
  let atLastNote = durationOfCounterPoint bb == durationOfCantusFirmus bb
      lastCFNote = getCurrentNote <=< back1 . end . cantusFirmus $ bb
      lastCPNote = getCurrentNote <=< back1 . end . counterPoint $ bb
      interval = liftM2 (#) lastCFNote lastCPNote
      isPerfectConsonance i = quality i == Perfect && simplify i `elem` [unison, perf5, octv]
      noteOk = toBool . liftM isPerfectConsonance $ interval
  in (if not atLastNote || noteOk then passTest else failTest) bb

-- TODO
fillInDim5OutlineAndOppStep = undefined
moreStepsThanSkips = undefined
avoidMaj6Skips = undefined
avoidMin6SkipsDown = undefined
precedeOrFollowSkipWithOppStep = undefined
dontUseMoreThan2SuccSkips = undefined
keep2SuccSkipsInSameDirSmall = undefined
pyramidRule = undefined
avoidSkipToAndFromLocalHighOrLow = undefined
