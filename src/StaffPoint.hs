module StaffPoint
( OctaveNum
, NoteLetter
, StaffPoint
, midC
, up, down
) where

import Data.Function (on)
import Control.Arrow
import Test.QuickCheck
import Control.Monad

type OctaveNum = Integer

data NoteLetter = C | D | E | F | G | A | B
  deriving (Eq, Ord, Show, Enum)

instance Arbitrary NoteLetter where
  arbitrary = elements [ C .. B ]

data StaffPoint = SP { letter :: NoteLetter, octnum :: OctaveNum }
  deriving Eq

instance Arbitrary StaffPoint where
  arbitrary = return SP `ap` arbitrary `ap` arbitrary

instance Show StaffPoint where
  show = uncurry (++) . (letter &&& octnum >>> show *** show)

instance Ord StaffPoint where
  compare a b = let x = (compare `on` octnum) a b in if x /= EQ then x else (compare `on` letter) a b

midC = SP C 4

up sp | letter sp == B  = SP C $ 1 + octnum sp
      | otherwise       = uncurry SP . (letter &&& octnum >>> first succ) $ sp

down sp | letter sp == C  = SP B $ octnum sp - 1
        | otherwise       = uncurry SP . (letter &&& octnum >>> first pred) $ sp

qc = quickCheckWith stdArgs { maxSuccess = 500, maxDiscard = 2500 }

test = do
  qc $ uncurry (==) . (up . down &&& id)
  qc $ uncurry (==) . (down . up &&& id)
