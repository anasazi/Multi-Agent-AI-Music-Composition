module StaffPoint
( OctaveNum
, NoteLetter(..)
, StaffPoint
, midC
, up, down
, letter, octnum
, test
) where

import Data.Function (on)
import Control.Arrow
import Test.QuickCheck
import Control.Monad

type OctaveNum = Integer

data NoteLetter = C | D | E | F | G | A | B
  deriving (Eq, Ord, Show, Enum)

next B = C
next x = succ x

instance Arbitrary NoteLetter where
  arbitrary = elements [ C .. B ]

data StaffPoint = SP { letter :: NoteLetter, octnum :: OctaveNum }
  deriving Eq

instance Arbitrary StaffPoint where
  arbitrary = liftM2 SP arbitrary arbitrary

instance Show StaffPoint where
  show = uncurry (++) . (letter &&& octnum >>> show *** show)

instance Ord StaffPoint where
  compare a b = let x = (compare `on` octnum) a b in if x /= EQ then x else (compare `on` letter) a b

midC = SP C 4

up sp | letter sp == B  = SP C $ 1 + octnum sp
      | otherwise       = uncurry SP . (succ . letter &&& octnum) $ sp

down sp | letter sp == C  = SP B $ octnum sp - 1
        | otherwise       = uncurry SP . (pred . letter &&& octnum) $ sp



qc = quickCheckWith stdArgs { maxSuccess = 500, maxDiscard = 2500 }

test = do
  qc $ uncurry (==) . (up . down &&& id)
  qc $ uncurry (==) . (down . up &&& id)
  qc $ uncurry (>) . (up &&& id)
  qc $ uncurry (<) . (down &&& id)