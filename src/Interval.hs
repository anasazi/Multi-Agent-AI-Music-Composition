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
	make :: (Note a, Interval b) => a -> a -> b -- make interval
	apply :: (Note a, Interval b) => b -> a -> a -- apply interval

data SimpleInterval = SI Size Quality
	deriving (Eq, Read, Show)

instance Arbitrary SimpleInterval where
	arbitrary = liftM2 SI arbitrary arbitrary

instance Interval SimpleInterval where
	size (SI s _) = sizeToInt s
	quality (SI _ q) = q
	width = undefined
	invert (SI s q) = SI (sizeInv s) (qualInv q)
	make = undefined
	apply = undefined

prop_invertSimpleInverse :: SimpleInterval -> Bool
prop_invertSimpleInverse s = s == invert (invert s)

data CompoundInterval = CI SimpleInterval Integer
	deriving (Eq, Read, Show)

instance Arbitrary CompoundInterval where
	arbitrary = liftM2 CI arbitrary arbitrary

instance Interval CompoundInterval where
	size = undefined
	quality = undefined
	width = undefined
	invert = undefined
	make = undefined
	apply = undefined
