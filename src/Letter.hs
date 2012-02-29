module Letter
( Letter(..)
, succL, predL
) where

import Test.QuickCheck
import Data.Function (on)

data Letter = A | B | C | D | E | F | G
	deriving (Eq, Show, Enum, Bounded)

instance Ord Letter where
	compare x y = (compare `on` length) (takeWhile (/=x) ls) (takeWhile (/=y) ls) 
		where ls = [ C, D, E, F, G, A, B ]

instance Arbitrary Letter where
	arbitrary = elements [ A, B, C, D, E, F, G ]

succL B = C
succL x = succ x

predL C = B
predL x = pred x
