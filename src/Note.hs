module Note
( Note
, midC
, halve, dot
, sharp, flat
, up, down
, pitch, dur
, test
) where

import Test.QuickCheck
import Control.Monad
import Control.Arrow

import qualified Pitch as P
import qualified Duration as D

data Note = N { pitch :: P.Pitch, dur :: D.Duration }
	deriving (Eq, Ord)

instance Show Note where
	show = pitch &&& dur >>> show *** show >>> second ('@':) >>> uncurry (++)

instance Arbitrary Note where
	arbitrary = liftM2 N arbitrary arbitrary

-- a middle C whole note
midC = N P.midC D.whole

durOp f = pitch &&& dur >>> second f >>> uncurry N
pitchOp f = pitch &&& dur >>> first f >>> uncurry N

-- lifted duration ops
halve = durOp D.halve
dot = durOp D.dot

-- lifted pitch ops
sharp = pitchOp P.sharp
flat = pitchOp P.flat
up =  pitchOp P.up
down = pitchOp P.down

-- causes monomorphism restriction error until used
--qc = quickCheckWith stdArgs { maxSuccess = 500, maxDiscard = 2500 }

test = do
	D.test
	P.test
	putStrLn "No real Note.hs tests"
