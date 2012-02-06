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

class Note a where
	midC :: a
	sharp :: a -> a
	flat :: a -> a
	normalizeAcci :: a -> a
	normalizeNote :: a -> a

nmove n f = foldl (.) id (replicate n f)

-- attempts to change note name
hup = normalizeNote . sharp
hdown = normalizeNote . flat

wup = nmove 2 hup
wdown = nmove 2 hdown

-- attempts to keep note name
hup' = normalizeAcci . sharp
hdown' = normalizeAcci . flat

wup' = nmove 2 hup'
wdown' = nmove 2 hdown'

octup = nmove 12 hup
octdown = nmove 12 hdown

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
							[Flat] -> name `elem` [A,B,D,F,G]
							otherwise -> False
		next (SN B (Sharp:als) oct) = SN C als (oct+1)
		next (SN C (Flat:als) oct) = SN B als (oct-1)
		next (SN E (Sharp:als) oct) = SN F als oct
		next (SN F (Flat:als) oct) = SN E als oct
		next (SN G (Sharp:Sharp:als) oct) = SN A als oct
		next (SN A (Flat:Flat:als) oct) = SN G als oct
		next (SN name (Sharp:Sharp:als) oct) = SN (succ name) als oct
		next (SN name (Flat:Flat:als) oct) = SN (pred name) als oct
