module NoteLetter
( NoteLetter(..)
) where

import Test.QuickCheck
import Data.Function (on)

data NoteLetter = A | B | C | D | E | F | G
	deriving (Eq, Show, Enum, Bounded)

instance Ord NoteLetter where
	compare x y = (compare `on` length) (takeWhile (/=x) ls) (takeWhile (/=y) ls) 
		where ls = [ C, D, E, F, G, A, B ]

instance Arbitrary NoteLetter where
	arbitrary = elements [ A, B, C, D, E, F, G ]
