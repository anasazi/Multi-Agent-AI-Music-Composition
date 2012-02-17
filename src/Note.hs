module Note 
( Note
, noteLetter
, nl
, octave
, oct
, accidentals
, acs
, midC
, (-=-)
, (->-)
, (-<-)
, (===)
, (=>=)
, (=<=)
, sharp
, flat
, up
, down
) where

import Test.QuickCheck
import Control.Monad
import qualified Data.List as L

-- Ordered in octave numbering order.
data NoteLetter = C | D | E | F | G | A | B deriving (Eq, Ord, Show, Enum)

instance Arbitrary NoteLetter where
	arbitrary = elements [ C .. B ]

type Octave = Integer

-- Represents a basic note on the staff
data StaffNote = SN { snNL :: NoteLetter, snOct :: Octave } deriving (Eq, Show)

instance Arbitrary StaffNote where
	arbitrary = liftM2 SN arbitrary arbitrary

instance Ord StaffNote where
	a <= b =  snOct a < snOct b || snOct a == snOct b && snNL a <= snNL b

snUp (SN B x) = SN C $ x+1
snUp (SN n x) = SN (succ n) x

snDown (SN C x) = SN B $ x-1
snDown (SN n x) = SN (pred n) x

snUpOct (SN n x) = SN n $ x+1
snDownOct (SN n x) = SN n $ x-1

testStaffNote = do
	quickCheck $ \n -> (==n) . snUp . snDown $ n
	quickCheck $ \n -> (==n) . snDown . snUp $ n
	quickCheck $ \n -> (==n) . snUpOct . snDownOct $ n
	quickCheck $ \n -> (==n) . snDownOct . snUpOct $ n

-- Represents accidentals as number of half steps off basic note
newtype Accidental = AC { runA :: Integer } deriving (Eq, Ord, Show)

instance Arbitrary Accidental where
	arbitrary = liftM AC arbitrary

acUp = AC . (+1) . runA
acDown = AC . (+(-1)) . runA

testAccidental = do
	quickCheck $ \a -> (==a) . acUp . acDown $ a
	quickCheck $ \a -> (==a) . acDown . acUp $ a

-- A note is made up of a basic note and an accidental
data Note = N StaffNote Accidental deriving (Eq, Ord, Show)
-- convenience functions
nSN (N x _) = x
nAC (N _ x) = x

instance Arbitrary Note where
	arbitrary = liftM2 N arbitrary arbitrary

-- provided so that a user can examine the traits of a note
noteLetter = snNL . nSN
nl = noteLetter
octave = snOct . nSN
oct = octave
accidentals = nAC
acs = accidentals

-- Provided so that a user can generate more notes
midC = N (SN C 4) (AC 0)

-- Staff Location Equality and Ordering
a -=- b = (nSN a) == (nSN b)
a ->- b = (==GT) $ compare (nSN a) (nSN b)
a -<- b = (==LT) $ compare (nSN a) (nSN b)

-- Notational Equality and Ordering, this is just the Eq or Ord instances of Note

-- Enharmonic Equality and Ordering
a === b | a -=- b = a == b | a -<- b = up a === b | a ->- b = a === up b
a =>= b | a -=- b = a > b  | a -<- b = up a =>= b | a ->- b = a =>= up b
a =<= b | a -=- b = a < b  | a -<- b = up a =<= b | a ->- b = a =<= up b

-- Change the pitch but not the basic note by modifying accidentals
sharp n = N (nSN n) (acUp . nAC $ n)
flat n = N (nSN n) (acDown . nAC $ n)

-- Change the basic note but not the pitch by counterbalencing with accidentals
up n
	| nl n `elem` [B,E] = N (snUp . nSN $ n) (acDown . nAC $ n)
	| otherwise					= N (snUp . nSN $ n) (acDown . acDown . nAC $ n)

down n
	| nl n `elem` [C,F] = N (snDown . nSN $ n) (acUp . nAC $ n)
	| otherwise					= N (snDown . nSN $ n) (acUp . acUp . nAC $ n)

testNote = do
	quickCheck $ \n -> (==n) . sharp . flat $ n
	quickCheck $ \n -> (==n) . flat . sharp $ n
	quickCheck $ \n -> (==n) . up . down $ n
	quickCheck $ \n -> (==n) . down . up $ n
	quickCheck $ \n -> (-=-n) . sharp . flat $ n
	quickCheck $ \n -> (-=-n) . up . down $ n
	quickCheck $ \n -> (->-n) . up $ n
	quickCheck $ \n -> (-<-n) . down $ n
	quickCheck $ \n -> (=>=n) . sharp $ n
	quickCheck $ \n -> (=<=n) . flat $ n
	quickCheck $ \n -> (sharp . down $ n) === (down . sharp $ n)
	quickCheck $ \n -> (flat . up $ n) === (up . flat $ n)
	quickCheck $ \n -> (sharp . up $ n) === (up . sharp $ n)
	quickCheck $ \n -> (flat . down $ n) === (down . flat $ n)

test = do
	testStaffNote
	testAccidental
	testNote
