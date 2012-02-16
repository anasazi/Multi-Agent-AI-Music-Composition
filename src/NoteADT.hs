module NoteADT where

import Test.QuickCheck
import qualified Data.List as L

data NoteLetter = C | D | E | F | G | A | B deriving (Eq, Ord, Enum, Bounded, Read, Show)
data Accidental = Flat | Sharp deriving (Eq, Ord, Enum, Bounded, Read, Show)
type Octave = Integer

instance Arbitrary NoteLetter where
  arbitrary = elements [C .. B]

instance Arbitrary Accidental where
  arbitrary = elements [Flat, Sharp]

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

  -- Staff line equality and ordering
  eqL :: n -> n -> Bool -- notes on the same staff line are equal
  ordL :: n -> n -> Ordering -- notes on lower staff lines are less

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

prop_sharpApplied n = [Sharp] == (ac (sharp n)) L.\\ (ac n) --Sharp == head (ac (sharp n))
prop_flatApplied n = [Flat] == (ac (flat n)) L.\\ (ac n) --Flat == head (ac (flat n))
prop_otupApplied n = 1 + ot n == ot (otup n)
prop_otdnApplied n = (-1) + ot n == ot (otdn n)

prop_normA_Imdepotent n = normA (normA n) == normA n
prop_sharp_flat_Inverse n = normA n == normA (sharp (flat n))
prop_flat_sharp_Inverse n = normA n == normA (flat (sharp n))
prop_otUpDown_Inverse n = ot n == ot (otup (otdn n))
prop_otDownUp_Inverse n = ot n == ot (otdn (otup n))
prop_nlUpDown_Inverse n = nl n == nl (nlup (nldn n))
prop_nlDownUp_Inverse n = nl n == nl (nldn (nlup n))

prop_eqN_Reverse n = (n `eqN` n)
prop_eqE_Reverse n = (n `eqE` n)
prop_eqL_Reverse n = (n `eqL` n)

prop_eqN_Implies_eqE n = (n `eqN` n) ==> (n `eqE` n)
prop_eqN_Implies_eqL n = (n `eqN` n) ==> (n `eqL` n)

prop_eqN_Implies_ordN n = (n `eqN` n) && (EQ == n `ordN` n)
prop_ordN_Implies_eqN a b = (a `ordN` b /= EQ) ==> not (a `eqN` b)

prop_ordN_Holds a b = (a `ordN` b == LT && b `ordN` a == GT) ||
                      (a `ordN` b == GT && b `ordN` a == LT) ||
                      (a `ordN` b == EQ && b `ordN` a == EQ && a `eqN` b)

prop_normA_eqE_Identity n = eqE n (normA n)
prop_sharp_flat_eqE_Identity n = eqE n (sharp (flat n))
prop_flat_sharp_eqE_Identity n = eqE n (flat (sharp n))

prop_ordE_Holds a b = (a `ordE` b == LT && b `ordE` a == GT) ||
                      (a `ordE` b == GT && b `ordE` a == LT) ||
                      (a `ordE` b == EQ && b `ordE` a == EQ && a `eqE` b)

prop_ordL_Holds a b = let ab = a `ordL` b; ba = b `ordL` a in 
                        (ab == LT && ba == GT) ||
                        (ab == GT && ba == LT) ||
                        (ab == EQ && ba == EQ && a `eqL` b)
