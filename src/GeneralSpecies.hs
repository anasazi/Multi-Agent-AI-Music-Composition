module GeneralSpecies
( agents
) where

import BlackboardSystem.KnowledgeSource
import BlackboardSystem.Blackboard
import Util.Zipper
import Music.Interval
import Music.Voice
import Music.Note
import Data.Function (on)

agents :: [ KnowledgeSource ]
agents = [ beginPerfectConsonance
         , noAugDimIntervals
         , noSkipLargerThan6
         ]

safeHead (x:_) = Just x
safeHead [] = Nothing

-- A general counterpoint hard rule: the first notes must be in a perfect consonance.
beginPerfectConsonance = makeHardRule op "General CP - start with perfect consonance."
  where op bb = let (cfStart:_) = escape (cantusFirmus bb)
                    (cpStart:_) = escape (counterPoint bb)
                    result = quality (pitch cfStart # pitch cpStart) == Perfect
                in (if result then passTest else failTest) bb
                --bb { testResult = quality (pitch cfStart # pitch cpStart) == Perfect }

noAugDimIntervals = makeHardRule op "General CP - no augmented or diminished intervals."
  where op bb = let curVZ = goToTime (counterPoint bb) (timeToTestAt bb)
                    preVZ = curVZ >>= back
                    interval = do cur <- curVZ >>= safeHead . focus
                                  pre <- preVZ >>= safeHead . focus
                                  let curPitch = pitch cur
                                  let prePitch = pitch pre
                                  --let curPitch = pitch (head (focus cur))
                                  --let prePitch = pitch (head (focus pre))
                                  return $ curPitch # prePitch
                    -- true if the interval is aug or dim
                    augOrDimMaybe = do int <- interval
                                       let q = quality int
                                       return $ case q of (Dim _) -> True
                                                          (Aug _) -> True
                                                          otherwise -> False
                    -- inverts the value of augOrDim if it exists
                    -- if it doesn't then we don't have an invalid interval, so we're good.
                    result = maybe True not augOrDimMaybe
                in (if result then passTest else failTest) bb 

noSkipLargerThan6 = makeHardRule op "General CP - no skips larger than a sixth (except octave)."
  where op bb = let curVZ = goToTime (counterPoint bb) (timeToTestAt bb)
                    preVZ = curVZ >>= back
                    intervalM = curVZ >>= safeHead . focus >>= \cur -> 
                                preVZ >>= safeHead . focus >>= \pre ->
                                return ( ((#) `on` pitch ) cur pre )
                    tooBigM = do int <- intervalM
                                 let size = lspan int
                                 return $ size > 6 && size /= 8
                    result = maybe True not tooBigM
                in  (if result then passTest else failTest) bb
