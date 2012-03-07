module Pitch
( Accidental
, Pitch
, point, off
, sharp, flat
, up, down
, midC
, test
) where

import Letter (Letter(..))
import qualified StaffPoint as SP

import Test.QuickCheck
import Control.Monad
import Control.Arrow
import Data.Function (on)

type Accidental = Integer

data Pitch = P { point :: SP.StaffPoint, off :: Accidental }

instance Arbitrary Pitch where
  arbitrary = liftM2 P arbitrary arbitrary

-- enharmonic equality
instance Eq Pitch where
  a == b = case (compare `on` point) a b of
            EQ -> ((==) `on` off) a b
            LT -> up a == b
            GT -> a == up b

-- enharmonic ordering
instance Ord Pitch where
  compare a b = case (compare `on` point) a b of
                  EQ -> (compare `on` off) a b
                  LT -> up a `compare` b
                  GT -> a `compare` up b

instance Show Pitch where
  show = uncurry (++) . (show . point &&& sign . off)
    where sign n | n >= 0 = "+"++show n | otherwise = show n

letter = SP.letter . point
octnum = SP.octnum . point

midC = P SP.midC 0

sharp = uncurry P . (point &&& (1+) . off)
flat = uncurry P . (point &&& (subtract 1) . off)
up p = let x = flat . uncurry P . (SP.up . point &&& off) $ p in
        if letter p `elem` [B, E] then x else flat x
down p = let x = sharp . uncurry P . (SP.down . point &&& off) $ p in
          if letter p `elem` [C, F] then x else sharp x

qc = quickCheckWith stdArgs { maxSuccess = 500, maxDiscard = 2500 }

test = SP.test >> putStrLn "Pitch.hs tests" >> do
  qc $ uncurry (==) . (sharp . flat &&& id) 
  qc $ uncurry (==) . (flat . sharp &&& id)
  qc $ uncurry (==) . (up . down &&& id)
  qc $ uncurry (==) . (down . up &&& id)
  qc $ uncurry (>) . (sharp &&& id)
  qc $ uncurry (<) . (flat &&& id)
  qc $ uncurry (==) . (down . sharp &&& sharp . down)
  qc $ uncurry (==) . (flat . up &&& up . flat)
  qc $ uncurry (==) . (down . flat &&& flat . down)
  qc $ uncurry (==) . (up . sharp &&& sharp . up)
