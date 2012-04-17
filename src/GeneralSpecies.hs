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
         ]


toBool = fromMaybe False

beginPerfectConsonance = makeHardRule op "General CP - start with perfect consonance."
  where
  op bb = let cfStart = getCurrentNote . front . cantusFirmus $ bb
              cpStart = getCurrentNote . front . cantusFirmus $ bb
              --result = quality (cfStart # cpStart) == Perfect
              isPerfect = (==Perfect) . quality
              result = toBool . liftM isPerfect . liftM2 (#) cfStart $ cpStart
          in (if timeToTestAt bb /= 0 || result then passTest else failTest) bb

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

-- TODO
noAug4Outline = undefined
fillInDim5OutlineAndOppStep = undefined
endPerfectConsonance = undefined
moreStepsThanSkips = undefined
avoidMaj6Skips = undefined
avoidMin6SkipsDown = undefined
precedeOrFollowSkipWithOppStep = undefined
dontUseMoreThan2SuccSkips = undefined
keep2SuccSkipsInSameDirSmall = undefined
pyramidRule = undefined
avoidSkipToAndFromLocalHighOrLow = undefined
