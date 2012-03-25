module Music.Pitch
( Accidental
, Pitch
, point, off
, sharp, flat
, up, down
, midC
, test
) where

import Music.Letter (Letter(..))
import qualified Music.StaffPoint as SP

import Test.QuickCheck
import Control.Monad
import Control.Arrow
import Data.Function (on)

type Accidental = Integer

--data Pitch = P { point :: SP.StaffPoint, off :: Accidental }
newtype Pitch = P { runP :: ( SP.StaffPoint, Accidental ) }

instance Arbitrary Pitch where
  arbitrary = liftM2 (curry P) arbitrary arbitrary

-- harmonic equality
instance Eq Pitch where
  a == b = case (compare `on` point) a b of
            EQ -> ((==) `on` off) a b
            LT -> up a == b
            GT -> a == up b

-- harmonic ordering
instance Ord Pitch where
  compare a b = case (compare `on` point) a b of
                  EQ -> (compare `on` off) a b
                  LT -> up a `compare` b
                  GT -> a `compare` up b

instance Show Pitch where
  show = runP >>> show *** showsign >>> uncurry (++)
    where showsign n | n >= 0 = '+' : show n | otherwise = show n

point = fst . runP
off = snd . runP

midC = P (SP.midC,0)

sharp = runP >>> second (+1) >>> P
flat  = runP >>> second (subtract 1) >>> P

up = flat >>> id &&& SP.letter . point >>> first (runP >>> first SP.up >>> P) >>> \(p,l) -> if l `elem` [B,E] then p else flat p 
down = sharp >>> id &&& SP.letter . point >>> first (runP >>> first SP.down >>> P) >>> \(p,l) -> if l `elem` [C,F] then p else sharp p

rep n = replicate (fromInteger n)

doN f n = foldl (.) id (rep n f)

qc :: Testable prop => prop -> IO ()
qc = quickCheckWith stdArgs { maxSuccess = 500, maxDiscard = 2500 }

test = SP.test >> putStrLn "Pitch.hs tests" >> do
  qc $ uncurry (==) . (up &&& id)
  qc $ uncurry (==) . (down &&& id)

  qc $ \a b -> ((a::Pitch) == (b::Pitch) && a <= b && a >= b) || (a /= b && (a < b || a > b))

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
