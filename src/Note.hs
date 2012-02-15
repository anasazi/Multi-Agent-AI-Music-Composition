module Note
( module NoteADT
) where

import NoteADT
import qualified Data.List as L
import Test.QuickCheck
import Control.Monad

data NoteName = A | B | C | D | E | F | G deriving (Eq, Enum, Show)

instance Arbitrary NoteName where
	arbitrary = elements [A .. G]

data Accidental = Flat | Sharp deriving (Eq, Ord, Show)

instance Arbitrary Accidental where
	arbitrary = elements [Flat, Sharp]

type Octave = Integer
data StdNote = SN { name :: NoteName 
									,	accis :: [Accidental] 
									,	oct :: Octave } deriving (Eq, Show)

instance Arbitrary StdNote where
	arbitrary = liftM3 SN arbitrary arbitrary arbitrary

instance Note StdNote where
	midC = SN C [] 4
	sharp n = n { accis = Sharp : accis n }
	flat n = n { accis = Flat : accis n }
	normA n = n { accis = L.unfoldr f . sum . concat . (map . map $ g) . L.group . L.sort $ accis n }
		where
		g a | a == Flat = (-1) | a == Sharp = 1
		f n | n == 0 = Nothing | n < 0 = Just (Flat, n+1) | n > 0 = Just (Sharp, n-1)
	normN n
		| null (accis n) = n
		| (accis n) == [Sharp] && (name n) `elem` [A,C,D,F,G] = n
		| (accis n) == [Flat] && (name n) `elem` [A,B,D,E,G] = n
		| otherwise = normN $ let n' = normA n in case n' of
										-- B & C, change octave cases
										(SN B (Sharp:acs) oct)	-> SN C acs (oct+1)
										(SN C (Flat:acs) oct)		-> SN B acs (oct-1)
										-- E & F half steps
										(SN E (Sharp:acs) oct)	-> SN F acs oct
										(SN F (Flat:acs) oct)		-> SN E acs oct
										-- A & G wrap around cases
										(SN G (Sharp:Sharp:acs) oct)	-> SN A acs oct
										(SN A (Flat:Flat:acs) oct)		-> SN G acs oct
										-- remainder
										(SN nme (Sharp:Sharp:acs) oct)	-> SN (succ nme) acs oct
										(SN nme (Flat:Flat:acs) oct)		-> SN (pred nme) acs oct
										-- it must be a case that's okay if normalized
										otherwise -> n'
		

tests :: [ StdNote -> Bool ]
tests = [ prop_normA_Idempotent
				, prop_normA_sharp_flat_Inverse
				, prop_normA_flat_sharp_Inverse
				, prop_normN_Idempotent
				, prop_normA_normN_Associative
				, (==Sharp) . head . accis . sharp
				, (==Flat) . head . accis . flat
				, \n -> let acs = accis (normA n) in (null acs) || all (==(head acs)) acs
				, \n -> oct n == (oct . normA $ n)
				, \n -> name n == (name . normA $ n)
				]
test = mapM_ quickCheck tests
