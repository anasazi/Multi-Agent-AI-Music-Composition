module Scale
( Scale
, makeScale
, BasedScale
, baseScale, (.$)
, allNotes, ($*)
, getNote, ($#)
, lenS
, inScale, ($.)
, inScaleAt, ($@)
, ($>), ($<)
, major, naturalMinor, natmin, harmonicMinor, harmin, melodicMinor, melmin
, ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian
) where

import Interval
import Pitch
import qualified Data.List as L

data Scale = S [Interval] deriving (Eq{-, Show-})
instance Show Scale where
	show (S is) = show is
makeScale = S

newtype BasedScale = BS { runBS :: (Scale, Pitch) } deriving (Eq)
instance Show BasedScale where
	show (BS (sc,bn)) = show sc ++ "@" ++ show bn
baseScale = curry BS
(.$) = baseScale

-- Given a scale and a base note, generate the sequence of notes in the scale
(S is) $* n = map (#^n) is
allNotes bs = let (sc,bn) = runBS bs in sc $* bn

-- Given a scale, a base note and an index, generate that note of the scale
bs $# idx = allNotes bs !! idx
getNote sc bn idx = (sc .$ bn)  $# idx -- just a rephrasing

-- Given a scale, get the length
lenS (S is) = length is

-- Given a base note and a test note, check if the test note is generated in the scale
bs $. n = n `elem` allNotes bs
inScale sc bn tn = (sc .$ bn) $. tn

-- Given a base note and a test note, check if the test note is generated in the scale and return the index
bs $@ n = n `L.elemIndex` allNotes bs
inScaleAt sc bn tn = (sc .$ bn) $@ tn

-- Given an index, get the next index (wrap around)
sc $> idx = (1 + idx) `mod` (lenS sc)

-- Given an index, get the previous index (wrap around)
sc $< idx = (idx - 1) `mod` (lenS sc)


------ Common Scales ---------

---- TONAL
major = makeScale [ unison, maj2, maj3, perf4, perf5, maj6, maj7, octave ]
naturalMinor = makeScale [ unison, maj2, min3, perf4, perf5, min6, min7, octave ]
natmin = naturalMinor
harmonicMinor = makeScale [ unison, maj2, min3, perf4, perf5, min6, maj7, octave ]
harmin = harmonicMinor
melodicMinor = makeScale [ unison, maj2, min3, perf4, perf5, maj6, maj7, octave ]
melmin = melodicMinor

---- MODAL
ionian = makeScale [ unison, maj2, maj3, perf4, perf5, maj6, maj7, octave ]
dorian = makeScale [ unison, maj2, min3, perf4, perf5, maj6, min7, octave ]
phrygian = makeScale [ unison, min2, min3, perf4, perf5, min6, min7, octave ]
lydian = makeScale [ unison, maj2, maj3, lengthen perf4{-aug4-}, perf5, maj6, maj7, octave ]
mixolydian = makeScale [ unison, maj2, maj3, perf4, perf5, maj6, min7, octave ]
aeolian = makeScale [ unison, maj2, min3, perf4, perf5, min6, min7, octave ]
locrian = makeScale [ unison, min2, min3, perf4, shorten perf5{-dim5-}, min6, min7, octave ]
