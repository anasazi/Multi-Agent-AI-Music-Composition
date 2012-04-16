module Scale
( Scale, runS, makeScale
, allNotes, getNote, inScale, inScaleAt
, major, naturalMinor, natmin, harmonicMinor, harmin, melodicMinor, melmin
, ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian
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

allNotes :: Scale -> Base -> [Note]
allNotes s b = map (#^b) (runS s)

getNote :: Scale -> Base -> Integer -> Note
getNote s b idx = allNotes s b !! fromIntegral idx

inScale :: Scale -> Base -> Note -> Bool
inScale s b n = n `elem` allNotes s b

inScaleAt :: Scale -> Base -> Note -> Maybe Integer
inScaleAt s b n = n `List.elemIndex` allNotes s b >>= return . fromIntegral


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
