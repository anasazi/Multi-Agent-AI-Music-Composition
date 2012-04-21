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
         , fillInDim5OutlineAndOppStep
         ]


toBool = fromMaybe False

-- General CP hard rule 2
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

-- General CP hard rule 3
beginPerfectConsonance = makeHardRule op "General CP - start with perfect consonance."
  where
  op bb = let cfStart = getCurrentNote . front . cantusFirmus $ bb
              cpStart = getCurrentNote . front . counterPoint $ bb
              interval = liftM2 (#) cfStart cpStart
              isPerfectConsonance i = quality i == Perfect && simplify i `elem` [unison, perf5, octv]
              noteOk = toBool . liftM isPerfectConsonance $ interval
              atFirstNote = timeToTestAt bb == 0
          in (if not atFirstNote || noteOk then passTest else failTest) bb

endPerfectConsonance = flip makeHardRule "General CP - end with perfect consonance." $ \bb ->
  let atLastNote = durationOfCounterPoint bb == durationOfCantusFirmus bb
      lastCFNote = getCurrentNote <=< back1 . end . cantusFirmus $ bb
      lastCPNote = getCurrentNote <=< back1 . end . counterPoint $ bb
      interval = liftM2 (#) lastCFNote lastCPNote
      isPerfectConsonance i = quality i == Perfect && simplify i `elem` [unison, perf5, octv]
      noteOk = toBool . liftM isPerfectConsonance $ interval
  in (if not atLastNote || noteOk then passTest else failTest) bb

-- General CP hard rule 4
noAug4Outline = flip makeHardRule "General CP - no outlines (interval btw local extrema) of augmented fourths." $ \bb ->
    let cp = goToTime (counterPoint bb) (timeToTestAt bb)
        ext1 = cp >>= recentLocalExtreme
        ext2 = ext1 >>= recentLocalExtreme
        interval = liftM2 (#) (ext1 >>= getCurrentNote) (ext2 >>= getCurrentNote)
        isAug4 = toBool . liftM (==aug4) $ interval
    in (if isAug4 then failTest else passTest) bb

fillInDim5OutlineAndOppStep = flip makeHardRule "General CP - an outline of dim5 must be completely filled in and followed by a step in the opposite direciton." $ \bb ->
  let cp = goToTime (counterPoint bb) (timeToTestAt bb)
      ext1 = cp >>= recentLocalExtreme
      ext2 = ext1 >>= recentLocalExtreme
      ext1Note = ext1 >>= getCurrentNote
      ext2Note = ext2 >>= getCurrentNote
      interval = liftM2 (#) ext1Note ext2Note
      isDim5 = toBool . liftM (==dim5) $ interval
      numBtw = fmap (subtract 1) $ liftM2 (-) (fmap numAfter ext2) (fmap numAfter ext1)
      lowerNote = liftM2 (min `on` Location) ext1Note ext2Note >>= \(Location note) -> Just note :: Maybe Note
      fillNotes = map (\n -> liftM (raise n) lowerNote) [1,2,3] :: [ Maybe Note ]
      intervening = do n <- numBtw
                       start <- ext2
                       (_:notes) <- getForwardN (n+1) start
                       return notes :: Maybe [Note]
      isFilled = toBool $ do interNotes <- intervening
                             let f ml = liftM (`elem` (map Location interNotes)) ml
                             let fillLocs = map (liftM Location) fillNotes
                             return . and . map toBool . map f $ fillLocs
      followingNote = ext1 >>= forward1 >>= getCurrentNote
      isStep = toBool $ liftM2 (#-#) ext1Note followingNote >>= Just . (==2) . abs
      -- We don't need to test if we're stepping in the opposite direction because we must be; otherwise ext1 would not be an extreme point.
  in (if not isDim5 || (isFilled && isStep) then passTest else failTest) bb

-- TODO
moreStepsThanSkips = undefined
avoidMaj6Skips = undefined
avoidMin6SkipsDown = undefined
precedeOrFollowSkipWithOppStep = undefined
dontUseMoreThan2SuccSkips = undefined
keep2SuccSkipsInSameDirSmall = undefined
pyramidRule = undefined
avoidSkipToAndFromLocalHighOrLow = undefined
