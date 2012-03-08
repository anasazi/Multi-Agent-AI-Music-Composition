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

newtype StaffPoint = SP { runSP :: ( Letter, OctaveNum ) }
  deriving Eq

letter = fst . runSP
octnum = snd . runSP

instance Arbitrary StaffPoint where
  arbitrary = liftM2 (curry SP) arbitrary arbitrary

instance Show StaffPoint where
  show = runSP >>> show *** show >>> uncurry (++)

swap (a,b) = (b,a)

instance Ord StaffPoint where
  compare = compare `on` (swap . runSP)

midC = SP (C,4)

type SPUnary = StaffPoint -> StaffPoint

-- go up one
up :: SPUnary
up = runSP >>> (\(l,o) -> if l == B then (B,o+1) else (l,o)) >>> first succL >>> SP

-- go down one
down :: SPUnary
down = runSP >>> (\(l,o) -> if l == C then (C,o-1) else (l,o)) >>> first predL >>> SP

qc = quickCheckWith stdArgs { maxSuccess = 500, maxDiscard = 2500 }

test = do
  Letter.test
  putStrLn "StaffPoint.hs tests"
  qc $ uncurry (==) . (up . down &&& id)
  qc $ uncurry (==) . (down . up &&& id)
  qc $ uncurry (>) . (up &&& id)
  qc $ uncurry (<) . (down &&& id)
  qc $ uncurry (==) . (id &&& (letter &&& octnum >>> SP))
