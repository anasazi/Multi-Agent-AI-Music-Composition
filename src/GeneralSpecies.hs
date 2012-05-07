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
         , moreStepsThanSkips
         , avoidMaj6Skips
         , avoidMin6SkipsDown
         , precedeOrFollowSkipWithOppStep 
         , dontUseMoreThan2SuccSkips
         , keep2SuccSkipsInSameDirSmall
         ]


toBool = fromMaybe False

-- skipping hard rule 1

-- hard rule 2
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

-- hard rule 3
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

-- hard rule 4
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
                             let f = liftM (`elem` map Location interNotes)
                             let fillLocs = map (liftM Location) fillNotes
                             return . all toBool . map f $ fillLocs
      followingNote = ext1 >>= forward1 >>= getCurrentNote
      isStep = toBool $ liftM2 (#-#) ext1Note followingNote >>= Just . (==2) . abs
      -- We don't need to test if we're stepping in the opposite direction because we must be; otherwise ext1 would not be an extreme point.
  in (if not isDim5 || (isFilled && isStep) then passTest else failTest) bb

-- soft rule 1
moreStepsThanSkips = flip makeSoftRule "General CP - use steps for frequently than skips." $ \bb ->
  let cpNotes = focus . front . counterPoint $ bb
      cpIntervals = zipWith (#) cpNotes (drop 1 cpNotes)
      isSkip = (>2) . lspan
      netSteps = sum . map (\i -> if isSkip i then (-1) else 1) $ cpIntervals
      -- going to go ahead and prefer a majority of steps
  in (if netSteps >= 0 then passTest else failTest) bb

-- soft rule 2
avoidMaj6Skips = flip makeSoftRule "General CP - avoid skipping a major sixth." $ \bb ->
  let cp = goToTime (counterPoint bb) (timeToTestAt bb)
      notes = getBackN 2 =<< cp
      interval = do [cur,bk1] <- notes
                    return $ cur # bk1
      isMaj6 = toBool . liftM (==maj6) $ interval
  in (if isMaj6 then failTest else passTest) bb

avoidMin6SkipsDown = flip makeSoftRule "General CP - avoid skipping a minor sixth downwards." $ \bb ->
  let cp = goToTime (counterPoint bb) (timeToTestAt bb)
      notes = getBackN 2 =<< cp
      interval = notes >>= \[cur,bk1] -> return $ cur # bk1
      isMin6 = toBool . liftM (==min6) $ interval
      isDesc = toBool $ notes >>= \[cur,bk1] -> return $ Location cur < Location bk1 
  in (if isMin6 && isDesc then failTest else passTest) bb

-- soft rule 3
precedeOrFollowSkipWithOppStep = flip makeSoftRule "General CP - prefer to precede and/or follow a skip with a step in opposite direction." $ \bb ->
  let cp = goToTime (counterPoint bb) (timeToTestAt bb)
      notes = getBackN 3 =<< cp
      intervals = notes >>= \[cur,bk1,bk2] -> return [cur # bk1, bk1 # bk2]
      isSkip = (>2) . lspan
      bothSkips = toBool $ fmap (all isSkip) intervals
      oneSkip = toBool $ fmap (any isSkip) intervals
      isOpp = toBool $ do [cur,bk1,bk2] <- notes
                          let isMin = Location cur > Location bk1 && Location bk1 < Location bk2
                          let isMax = Location cur < Location bk1 && Location bk1 > Location bk2
                          return $ isMin || isMax
  in (if bothSkips || oneSkip && not isOpp then failTest else passTest) bb

-- soft rule 4
dontUseMoreThan2SuccSkips = flip makeSoftRule "General CP - do not use more than 2 skips in succession." $ \bb ->
  let cp = goToTime (counterPoint bb) (timeToTestAt bb)
      notes = cp >>= getBackN 4 
      intervals = liftM (\ns -> zipWith (#) ns (drop 1 ns)) notes 
      isSkip = (>2) . lspan
      allSkips = toBool $ fmap (all isSkip) intervals
  in (if allSkips then failTest else passTest) bb

-- soft rule 5
keep2SuccSkipsInSameDirSmall = flip makeSoftRule "General CP - if there are 2 successive skips in the same direction, keep them small (<4th)." $ \bb ->
  let cp = goToTime (counterPoint bb) (timeToTestAt bb)
      notes = cp >>= getBackN 3
      intervals = liftM (\ns -> zipWith (#) ns (drop 1 ns)) notes
      isSkip = (>2) . lspan
      isSmall = (<4) . lspan
      inSameDir = toBool $ do [cur,bk1,bk2] <- notes
                              let isUp = Location bk2 < Location bk1 && Location bk1 < Location cur
                              let isDn = Location bk2 > Location bk1 && Location bk1 > Location cur
                              return $ isUp || isDn
      bothSkips = toBool $ fmap (all isSkip) intervals --intervals >>= return . all isSkip
      bothSmall = toBool $ fmap (all isSmall) intervals -- intervals >>= return . all isSmall
  in (if bothSkips && inSameDir && not bothSmall then failTest else passTest) bb

-- soft rule 6
pyramidRule = flip makeSoftRule "General CP - when using skips and steps in the same direction, larger intervals should be below smaller ones." $ \bb ->
  undefined

-- soft rule 7
avoidSkipToAndFromLocalHighOrLow = flip makeSoftRule "General CP - avoid skipping both to and from a temporary high or low point." $ \bb ->
  undefined

-- skipping soft rule 8
