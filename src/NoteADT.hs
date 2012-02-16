module NoteADT where

data NoteLetter = A | B | C | D | E | F | G deriving (Eq, Ord, Enum, Bounded, Read, Show)
data Accidental = Flat | Sharp deriving (Eq, Ord, Enum, Bounded, Read, Show)
type Octave = Integer

class Note n where
	-- Middle C provides a base note that can be used to generate other notes.
	midC :: n

	-- Functions to extract information about a Note for analysis
	nl :: n -> NoteLetter -- get the note letter
	ac :: n -> [Accidental] -- get the accidental list
	ot :: n -> Octave -- the octave number

	-- Notational equality and ordering
	eqN :: n -> n -> Bool -- True if both notes have the same notation
	ordN :: n -> n -> Ordering -- Orders notes based on their positioning on a staff. For notes on the same line, accidentals determine the ordering.

	-- Enharmonic equality and ordering
	eqE :: n -> n -> Bool -- True if both notes represent the same pitch
	ordE :: n -> n -> Ordering -- Orders notes based on the pitches they represent

	-- Functions to build new notes from existing notes (do not simplify representation)
	sharp :: n -> n -- raise a note by a half step
	flat :: n -> n -- lower a note by a half step
	otup :: n -> n -- raise a note by an octave (convenient)
	otdn :: n -> n -- lower a ntoe by an octave (convenient)
	
	-- Functions to modify the note letter and counterbalancing with accidentals. This is useful for controlling interval size
	nlup :: n -> n -- go up 
	nldn :: n -> n -- go down 

	-- Functions to simplify the representation of a note
	normA :: n -> n -- consolidate accidentals
	-- TODO how do we handle a case like C#/Db
	-- normN :: n -> n -- form simplest representation

	-- TODO add handling for duration
