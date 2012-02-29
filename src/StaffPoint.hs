module StaffPoint
( StaffPoint
, midC
, up, down
, letter, octnum
, test
) where

import Data.Function (on)
import Control.Arrow
import Test.QuickCheck
import Control.Monad

import Letter hiding (test)
import qualified Letter

type OctaveNum = Integer

data StaffPoint = SP { letter :: Letter, octnum :: OctaveNum }
  deriving Eq

instance Arbitrary StaffPoint where
  arbitrary = liftM2 SP arbitrary arbitrary

instance Show StaffPoint where
  show (SP l o) = show l ++ show o
  --show = uncurry (++) . (letter &&& octnum >>> show *** show)

instance Ord StaffPoint where
  compare a b = let x = (compare `on` octnum) a b in if x /= EQ then x else (compare `on` letter) a b

toPair = letter &&& octnum

midC = SP C 4

up = uncurry SP . (nextL &&& (toPair >>> first (==B) >>> opOct))
  where 
  nextL = letter >>> succL
  opOct p = snd p + (if fst p then 1 else 0)

down = uncurry SP . (prevL &&& (toPair >>> first (==C) >>> opOct))
  where
  prevL = letter >>> predL
  opOct p = snd p - (if fst p then 1 else 0)

qc = quickCheckWith stdArgs { maxSuccess = 500, maxDiscard = 2500 }

test = do
  Letter.test
  putStrLn "StaffPoint.hs tests"
  qc $ uncurry (==) . (up . down &&& id)
  qc $ uncurry (==) . (down . up &&& id)
  qc $ uncurry (>) . (up &&& id)
  qc $ uncurry (<) . (down &&& id)
