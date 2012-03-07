module Duration
( Duration
, halve, dot
, whole, half, quarter, eighth
, test
) where

import Data.Function (on)
import Test.QuickCheck
import Control.Monad
import Control.Arrow

type Halves = Integer
type Dots = Integer
data Duration = D { halves :: Halves, dots :: Dots }
	deriving Eq

dur = halves &&& dots >>> ((1/2)^) *** ((3/2)^) >>> uncurry (*)

instance Ord Duration where
	compare = compare `on` dur

instance Show Duration where
	show = halves &&& dots >>> show *** show >>> (++"h") *** (++"d") >>> uncurry (++)

instance Arbitrary Duration where
	arbitrary = liftM2 D (arbitrary `suchThat` (>=0)) (arbitrary `suchThat` (>=0))

-- operations
halve = halves &&& dots >>> first (+1) >>> uncurry D
dot = halves &&& dots >>> second (+1) >>> uncurry D

-- common values
whole = D 0 0
half = D 1 0
quarter = D 2 0
eighth = D 3 0

test = do
	putStrLn "Duration.hs tests"
	quickCheck $ (>0) . dur
	quickCheck $ uncurry (<) <<< halve &&& id
	quickCheck $ uncurry (>) <<< dot &&& id
	quickCheck $ uncurry (==) <<< halve . dot &&& dot . halve
