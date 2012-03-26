module Music.Voice
( Voice, VoiceZipper
, startTimeOfFocus, durationOfFocus, durationOfVoice
, goToTime, goToTime'
) where

import Music.Note 
import Util.Zipper
import qualified Music.Duration as D

{- At its core, a musical voice is a sequence of notes.
If we assume that the notes don't overlap, then we can map R+ to the voice. This is useful for lookup.

We'll examine voices through a zipper, which enables easy movement. 
For efficiency, we probably want to keep the start time of a note instead of recomputing it each time.

-}

-- first element of the list is the first note
type Voice = [ Note ]

type VoiceZipper = Zipper Note

-- TODO instead of a straight up list of notes, maybe we should tag them with start times

startTimeOfFocus :: VoiceZipper -> Double
startTimeOfFocus = sum . map (D.dur . dur) . context

durationOfFocus :: VoiceZipper -> Double
durationOfFocus = sum . map (D.dur . dur) . focus

durationOfVoice :: VoiceZipper -> Double
durationOfVoice = durationOfFocus . front

goToTime :: VoiceZipper -> Double -> Maybe VoiceZipper
goToTime vz t = f (front vz) 0
  where f vz acc | acc > t   = back vz
                 | acc == t = Just vz
                 | otherwise = forward vz >>= \vz -> f vz (acc + D.dur (dur (head (focus vz)))) 

goToTime' :: VoiceZipper -> Double -> VoiceZipper
goToTime' vz t = f (front vz) 0
  where f vz acc | acc > t   = back' vz
                 | acc == t  = vz
                 | otherwise = f (forward' vz) (acc + D.dur (dur (head (focus vz)))) 
