--{-# LANGUAGE NoMonomorphismRestriction #-}

{-
The Interval module provides an ADT representing musical intervals (the distance between 2 notes).

Intervals have size (difference in note names), and quality (major, minor, perfect, etc.).
Size and quality combined specify the width (difference in half steps).

Intervals larger than an octave are called compound.
Otherwise, Intevals are called simple.
Compound intervals can be expressed as a simple interval and a number of octaves.

Intervals can be inverted.
Given two Notes, an Interval can be constructed.
Given a Note and an Interval, a second Note can be constructed.
-}

import Note
import Test.QuickCheck
import Control.Monad

test = do
	quickCheck prop_sizeInv
	quickCheck prop_qualInv
	quickCheck prop_invertSimpleInverse
	quickCheck prop_invertCompoundInverse
	quickCheck prop_widthSimpleNonNeg
	quickCheck prop_widthCompoundNonNeg

data Quality = Diminished | Minor | Perfect | Major | Augmented
	deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Arbitrary Quality where
	arbitrary = elements [Diminished .. Augmented]

qualInv :: Quality -> Quality
qualInv = toEnum . (4-) . fromEnum

prop_qualInv :: Quality -> Bool
prop_qualInv q = q == (qualInv (qualInv q))

data Size = S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8
	deriving (Eq, Ord, Read, Show, Enum, Bounded)

sizeToInt :: Size -> Integer
sizeToInt = (1+) . toInteger . fromEnum

sizeInv :: Size -> Size
sizeInv = toEnum . fromInteger . (+(-1)) . (9-) . sizeToInt

prop_sizeInv :: Size -> Bool
prop_sizeInv s = s == sizeInv (sizeInv s)

instance Arbitrary Size where
	arbitrary = elements [S1 .. S8]

class Interval a where
	size :: a -> Integer -- difference in note names
	quality :: a -> Quality -- quality of interval
	width :: Interval a => a -> Integer -- difference in half steps
	invert :: Interval a => a -> a -- the inversion of the interval
	make :: Note b => b -> b -> a -- make interval
	apply :: Note b => a -> b -> b -- apply interval

data SimpleInterval = SI Size Quality
	deriving (Eq, Read, Show)

validSimpleIntervals :: [SimpleInterval]
validSimpleIntervals = concatMap f $ [ SI S1 Perfect ] ++
																		 [ SI s q | s <- [S2,S3], q <-[Minor,Major] ] ++
																		 [ SI S4 Perfect, SI S5 Perfect ] ++
																		 [ SI s q | s <- [S6,S7], q <-[Minor,Major] ] ++
																		 [ SI S8 Perfect ]
	where 
	f i@(SI S1 _) = [i, SI S1 Augmented]
	f i@(SI S8 _) = [SI S8 Diminished, i]
	f i@(SI s Perfect) = [SI s Diminished, i, SI s Augmented]
	f i@(SI s Minor) = [SI s Diminished, i]
	f i@(SI s Major) = [i, SI s Augmented]

instance Arbitrary SimpleInterval where
	arbitrary = elements validSimpleIntervals
	--arbitrary = liftM2 SI arbitrary arbitrary

instance Interval SimpleInterval where
	size (SI s _) = sizeToInt s
	quality (SI _ q) = q
	width (SI S1 Perfect) = 0
	width (SI S2 Minor)		= 1
	width (SI S2 Major)		= 2
	width (SI S3 Minor)		= 3
	width (SI S3 Major)		= 4
	width (SI S4 Perfect)	= 5
	width (SI S5 Perfect) = 7
	width (SI S6 Minor)		= 8
	width (SI S6 Major)		= 9
	width (SI S7 Minor)		= 10
	width (SI S7 Major)		= 11
	width (SI S8 Perfect)	= 12
	width (SI s Diminished)
		| s `elem` [S4,S5,S8] = width (SI s Perfect) - 1
		| s == S1							= undefined
		| otherwise						= width (SI s Minor) - 1
	width (SI s Augmented)
		| s `elem` [S1,S4,S5]	= width (SI s Perfect) + 1
		| s == S8							= undefined
		| otherwise						= width (SI s Major) + 1
	invert (SI s q) = SI (sizeInv s) (qualInv q)
	apply i@(SI s q) n = nmove hup (width i) n -- TODO doesn't work
	make = undefined

prop_invertSimpleInverse :: SimpleInterval -> Bool
prop_invertSimpleInverse s = s == invert (invert s)

prop_widthSimpleNonNeg :: SimpleInterval -> Bool
prop_widthSimpleNonNeg s = width s >= 0

data CompoundInterval = CI SimpleInterval Integer
	deriving (Eq, Read, Show)

instance Arbitrary CompoundInterval where
	arbitrary = liftM2 CI arbitrary (suchThat arbitrary (>0))

instance Interval CompoundInterval where
	size (CI si o) = 7*o + size si
	quality (CI si _) = quality si
	width (CI si o) = 12*o + width si
	invert (CI si o) = CI (invert si) o
	make = undefined
	apply = undefined

prop_invertCompoundInverse :: CompoundInterval -> Bool
prop_invertCompoundInverse c = c == invert (invert c)

prop_widthCompoundNonNeg :: CompoundInterval -> Bool
prop_widthCompoundNonNeg c = width c >= 0
