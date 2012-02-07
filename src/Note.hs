{-# LANGUAGE NoMonomorphismRestriction #-}

{-
Note ADT interface:
	midC -- returns a Note representing middle C
	sharp -- applies a sharp
	flat -- applies a flat
	normalizeNote -- simplfy to a single accidental (possibly change note name)
	normalizeAcci -- simplfy sharps and flats as much as possible

	moving half steps while attempting to change note name (at most one accidental)
			example: C -> Db, B -> C, Ab -> A
		hup -- up a half step
		hdown -- down a half step

	moving half steps while attempting to keep note name (at most one accidental)
			example: C -> C#, B -> B#, G# -> A
		hup' -- up a half step
		hdown' -- down a half step

	special motions based on above:
		whole step (wup, ...)
		octaves (octup, ...)
		n of some motion
-}

module Note 
( Note
, StdNote()
, nmove
, hup
, hdown
, wup
, wdown
, hup'
, hdown'
, wup'
, wdown'
, octup
, octdown
) where

import Test.QuickCheck
import Control.Monad

class Note a where
	midC :: a
	sharp :: a -> a
	flat :: a -> a
	normalizeAcci :: a -> a
	normalizeNote :: a -> a

nmove f n = foldl (.) id (replicate n f)

-- attempts to change note name
hup = normalizeNote . sharp
hdown = normalizeNote . flat

wup = nmove hup 2
wdown = nmove hdown 2

-- attempts to keep note name
hup' = normalizeAcci . sharp
hdown' = normalizeAcci . flat

wup' = nmove hup' 2
wdown' = nmove hdown' 2

octup = nmove hup 12
octdown = nmove hdown 12

data NoteName = A | B | C | D | E | F | G deriving (Eq, Ord, Read, Show, Enum, Bounded)
data Accidental = Flat | Sharp deriving (Eq, Ord, Read, Show, Enum, Bounded)
type Octave = Integer
data StdNote = SN NoteName [Accidental] Octave deriving (Eq, Read, Show)

instance Note StdNote where
	midC = SN C [] 4
	sharp (SN name als oct) = SN name (Sharp:als) oct
	flat (SN name als oct) = SN name (Flat:als) oct
	normalizeAcci (SN name als oct) = SN name normed oct
		where
		net = sum $ map f als
			where
			f Sharp = 1
			f Flat = -1
		normed
			| net < 0 = replicate (abs net) Flat
			| net > 0 = replicate net Sharp
			| otherwise = []
	normalizeNote n 
		| naGood = na
		| otherwise = normalizeNote (next na)
		where
		na@(SN name als oct) = normalizeAcci n
		naGood = case als of
							[] -> True
							[Sharp] -> name `elem` [A,C,D,F,G]
							[Flat] -> name `elem` [A,B,D,E,G]
							otherwise -> False
		next (SN B (Sharp:als) oct) = SN C als (oct+1)
		next (SN C (Flat:als) oct) = SN B als (oct-1)
		next (SN E (Sharp:als) oct) = SN F als oct
		next (SN F (Flat:als) oct) = SN E als oct
		next (SN G (Sharp:Sharp:als) oct) = SN A als oct
		next (SN A (Flat:Flat:als) oct) = SN G als oct
		next (SN name (Sharp:Sharp:als) oct) = SN (succ name) als oct
		next (SN name (Flat:Flat:als) oct) = SN (pred name) als oct

instance Arbitrary NoteName where
	arbitrary = elements [A .. G]

instance Arbitrary Accidental where
	arbitrary = elements [Flat, Sharp]

instance Arbitrary StdNote where
	arbitrary = liftM3 SN arbitrary arbitrary arbitrary

prop_sharpApplied :: StdNote -> Bool
prop_sharpApplied n = let (SN _ als _) = sharp n in head als == Sharp

prop_flatApplied :: StdNote -> Bool
prop_flatApplied n = let (SN _ als _) = flat n in head als == Flat

prop_normalizeAcciAllSame :: StdNote -> Bool
prop_normalizeAcciAllSame n = let (SN _ als _) = normalizeAcci n in (null als) || all (==(head als)) als

prop_normalizeAcciNameOctConst :: StdNote -> Bool
prop_normalizeAcciNameOctConst n@(SN name _ oct) = let (SN name' _ oct') = normalizeAcci n in name == name' && oct == oct'

prop_normalizeAcciImdempotent :: StdNote -> Bool
prop_normalizeAcciImdempotent n = normalizeAcci (normalizeAcci n) == normalizeAcci n

prop_normalizeNoteImdempotent :: StdNote -> Bool
prop_normalizeNoteImdempotent n = normalizeNote (normalizeNote n) == normalizeNote n

prop_hup'Inverse :: StdNote -> Bool
prop_hup'Inverse n = (normalizeAcci n) == hup' (hdown' n)

test = do
	quickCheck prop_sharpApplied
	quickCheck prop_flatApplied
	quickCheck prop_normalizeAcciAllSame
	quickCheck prop_normalizeAcciNameOctConst
	quickCheck prop_normalizeAcciImdempotent
	quickCheck prop_normalizeNoteImdempotent
	quickCheck prop_hup'Inverse
