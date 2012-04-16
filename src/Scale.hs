module Scale
( Scale, runS, makeScale
, allNotes, getNote, inScale, inScaleAt
, major, naturalMinor, natmin, harmonicMinor, harmin, melodicMinor, melmin
, ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian
, BasedScale, runBS, scale, base, baseScale, (.$)
, ($*), ($#), ($.), ($@)
) where

import Interval
import Note (Note)
import qualified Data.List as List

newtype Scale = S [Interval] deriving Eq
runS (S is) = is
makeScale = S
instance Show Scale where
  show = show . runS

type Base = Note

newtype BasedScale = BS (Scale, Note) deriving Eq
instance Show BasedScale where
  show bs = let (s,b) = runBS bs in show s ++ "@" ++ show b
runBS (BS x) = x
scale (BS (s,_)) = s
base (BS (_,b)) = b
baseScale = BS
(.$) = baseScale

allNotes :: Scale -> Base -> [Note]
allNotes s b = map (#^b) (runS s)
($*) = allNotes

getNote :: Scale -> Base -> Integer -> Note
getNote s b idx = allNotes s b !! fromIntegral idx
bs $# idx = let (s,b) = runBS bs in getNote s b idx

inScale :: Scale -> Base -> Note -> Bool
inScale s b n = n `elem` allNotes s b
bs $. n = let (s,b) = runBS bs in inScale s b n

inScaleAt :: Scale -> Base -> Note -> Maybe Integer
inScaleAt s b n = n `List.elemIndex` allNotes s b >>= return . fromIntegral
bs $@ n = let (s,b) = runBS bs in inScaleAt s b n


------ Common Scales ---------

---- TONAL
major = makeScale [ unison, maj2, maj3, perf4, perf5, maj6, maj7, octv ]
naturalMinor = makeScale [ unison, maj2, min3, perf4, perf5, min6, min7, octv ]
natmin = naturalMinor
harmonicMinor = makeScale [ unison, maj2, min3, perf4, perf5, min6, maj7, octv ]
harmin = harmonicMinor
melodicMinor = makeScale [ unison, maj2, min3, perf4, perf5, maj6, maj7, octv ]
melmin = melodicMinor

---- MODAL
ionian = makeScale [ unison, maj2, maj3, perf4, perf5, maj6, maj7, octv ]
dorian = makeScale [ unison, maj2, min3, perf4, perf5, maj6, min7, octv ]
phrygian = makeScale [ unison, min2, min3, perf4, perf5, min6, min7, octv ]
lydian = makeScale [ unison, maj2, maj3, lengthen 1 perf4{-aug4-}, perf5, maj6, maj7, octv ]
mixolydian = makeScale [ unison, maj2, maj3, perf4, perf5, maj6, min7, octv ]
aeolian = makeScale [ unison, maj2, min3, perf4, perf5, min6, min7, octv ]
locrian = makeScale [ unison, min2, min3, perf4, shorten 1 perf5{-dim5-}, min6, min7, octv ]
