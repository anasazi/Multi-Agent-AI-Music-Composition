module Note 
( Note()
, midC
, halfStepUp
, halfStepDown
, wholeStepUp
, wholeStepDown
) where

{-
The Note module provides an ADT representing musical notes in the form of pitch class and octave pairs.

Middle C is provided as a base note via the midC value.
Other notes can be created via half and whole steps up and down.

Note is an instance of Eq, Ord, Show, Read, Enum, and Bounded.
-}

import Test.QuickCheck
import Control.Monad

data Note = Note Octave PitchClass deriving (Eq,Ord,Show,Read,Bounded)
data PitchClass = PCC | PCCsDb | PCD | PCDsEb | PCE | PCF | PCFsGb | PCG | PCGsAb | PCA | PCAsBb | PCB deriving (Eq,Ord,Show,Enum,Read,Bounded)
data Octave = O0 | O1 | O2 | O3 | O4 | O5 | O6 | O7 | O8 deriving (Eq,Ord,Show,Enum,Read,Bounded)

allPitchClasses = [PCC .. PCB]
allOctaves = [O0 .. O8]
allNotes = [ Note oct pc | oct <- allOctaves, pc <- allPitchClasses ]

midC = Note O4 PCC

instance Enum Note where
	toEnum n = allNotes !! n
	fromEnum x = length $ takeWhile (/=x) allNotes

halfStepUp, halfStepDown, wholeStepUp, wholeStepDown :: Note -> Note
halfStepUp = succ
halfStepDown = pred
wholeStepUp = succ . succ
wholeStepDown = pred . pred

-- testing code

instance Arbitrary Octave where
	arbitrary = elements allOctaves

instance Arbitrary PitchClass where
	arbitrary = elements allPitchClasses

instance Arbitrary Note where
	arbitrary = liftM2 Note arbitrary arbitrary

prop_halfStepInverse note = not (note == minBound || note == maxBound) ==> note == halfStepUp (halfStepDown note)
prop_wholeStepInverse note = not (note <= succ minBound || note >= pred maxBound) ==> note == wholeStepUp (wholeStepDown note)

props = [ prop_halfStepInverse, prop_wholeStepInverse ]

test = mapM_ quickCheck props
