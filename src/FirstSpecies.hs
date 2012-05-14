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
import qualified Data.List as L
import Control.Applicative

type Time = Double

agents :: [Agent]
agents = [ firstSpeciesGenerator
         , consonantDownbeats
         , approachPerfIntervalsByContraryOrOblique 
         , noSimulRepeatsInBothCFAndCP 
         , max4NoteParallelRun 
         , skipsLessThanHalf 
         , avoidMoreThan2PerfVertAdjacent 
         , avoidUnisonExceptStartAndEnd 
         , avoidSimulSkipsUnlessSmall 
         , avoidWideSeparation 
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
  let cf = goToTime (cantusFirmus bb) (timeToTestAt bb)
      cp = goToTime (counterPoint bb) (timeToTestAt bb)
      cfNotes = cf >>= getBackN 5
      cpNotes = cp >>= getBackN 5
      intervals = liftM2 (zipWith (#)) cfNotes cpNotes
      allEqual = toBool $! liftM ((==1) . length . L.group) intervals
  in if not (isFirstSpecies bb (timeToTestAt bb)) || not allEqual then passTest bb else failTest bb

-- hard rule 5
skipsLessThanHalf = flip makeHardRule "First species - Skips must be less than half of melodic motions." $ \bb ->
  let cp = goToTime (counterPoint bb) (timeToTestAt bb)
      notes = cp >>= \cpV -> getCurrentNote cpV >>= \cur -> Just (cur : context cpV)
      intervals = liftM (\x -> zipWith (#) x (drop 1 x)) notes
      isSkip = (>2) . lspan
      numSkips = liftM (length . filter isSkip) intervals
      numCFmotions = length . focus . front . cantusFirmus $! bb
      tooMany = toBool $! liftM ((>= numCFmotions) . (*2)) numSkips
  in if not (isFirstSpecies bb (timeToTestAt bb)) || not tooMany then passTest bb else failTest bb

-- hard rule 6 NYI
noDirectRepetition = {-flip makeHardRule "First species - Do not repeat a sequence of at least 2 motions and intervals without at least two intervening whole notes." $ \bb ->
  let cf = goToTime (cantusFirmus bb) (timeToTestAt bb)
      cp = goToTime (counterPoint bb) (timeToTestAt bb)
      cfNotes = liftM2 (:) (cf >>= getCurrentNote) (fmap context cf)
      cpNotes = liftM2 (:) (cp >>= getCurrentNote) (fmap context cp)
      isSyncd = liftM2 (on (==) startTimeOfFocus) cf cp
      isFirstSpeciesSection cfs cps = allWhole cfs && allWhole cps
        where allWhole = all (on (==) Duration whole)
     -- isValidSection = (&&) <$> even <*> (>4)
  in-} undefined
only2SequentialRepetition = undefined

-- soft rule 1
avoidMoreThan2PerfVertAdjacent = flip makeSoftRule "First species - Avoid more than two perfect vertical intervals in a row." $ \bb ->
  let cf = goToTime (cantusFirmus bb) (timeToTestAt bb)
      cp = goToTime (counterPoint bb) (timeToTestAt bb)
      cfNotes = cf >>= getBackN 3
      cpNotes = cp >>= getBackN 3
      intervals = liftM2 (zipWith (#)) cfNotes cpNotes :: Maybe [Interval]
      tooManyPerfect = toBool $ liftM (all ((==Perfect) . quality)) intervals
  in if not (isFirstSpecies bb (timeToTestAt bb)) || not tooManyPerfect then passTest bb else failTest bb
  
-- soft rule 2
avoidUnisonExceptStartAndEnd = flip makeSoftRule "First species - Avoid vertical unisons except on the first and last notes." $ \bb ->
  let cf = goToTime (cantusFirmus bb) (timeToTestAt bb) >>= getCurrentNote
      cp = goToTime (counterPoint bb) (timeToTestAt bb) >>= getCurrentNote
      isUnison = toBool $ liftM (==unison) $ liftM2 (#) cf cp
      okTime = toBool $ (||) <$> isStart <*> isEnd <$> goToTime (counterPoint bb) (timeToTestAt bb)
  in if not (isFirstSpecies bb (timeToTestAt bb)) || okTime || not isUnison then passTest bb else failTest bb

-- soft rule 3
avoidSimulSkipsUnlessSmall = flip makeSoftRule "First species - Avoid simultaneous skips in both voices unless both only move a third." $ \bb ->
  let cf = goToTime (cantusFirmus bb) (timeToTestAt bb) >>= getBackN 2 >>= \[cur,bk1] -> return $ cur # bk1
      cp = goToTime (counterPoint bb) (timeToTestAt bb) >>= getBackN 2 >>= \[cur,bk1] -> return $ cur # bk1
      isTooBigSkip = (>3) . lspan
      bigSimulSkips = toBool $ do cfI <- cf
                                  cpI <- cp
                                  return $ isTooBigSkip cfI && isTooBigSkip cpI
  in if not (isFirstSpecies bb (timeToTestAt bb)) || not bigSimulSkips then passTest bb else failTest bb

-- soft rule 4
avoidWideSeparation = flip makeSoftRule "First species - Avoid separating voices more than a twelth." $ \bb ->
  let cf = goToTime (cantusFirmus bb) (timeToTestAt bb) >>= getCurrentNote
      cp = goToTime (counterPoint bb) (timeToTestAt bb) >>= getCurrentNote
      interval = liftM2 (#) cf cp
      tooFarApart = toBool $ fmap ((>12) . lspan) interval
  in if not (isFirstSpecies bb (timeToTestAt bb)) || not tooFarApart then passTest bb else failTest bb

-- soft rule 5
preferDirChangeAfterLargeSkipAndFollowWithStep = undefined
-- soft rule 6
tryToConnectExtremesWithSteps = undefined
-- soft rule 7
coverWholeOctaveEvery10To20Notes = undefined
