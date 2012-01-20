module Note 
( Note()
, midC
, halfStepUp
, halfStepDown
, wholeStepUp
, wholeStepDown
) where

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
