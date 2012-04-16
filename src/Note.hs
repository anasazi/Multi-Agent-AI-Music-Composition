module Note
( Letter(..) , Octave , Accidental , Halves , Dots -- Elements of a note
, Note -- the Note type
, letter, octave, accidental, halves, dots -- accessors for a note
, makeNote -- constructor
, durAsNum -- view duration as a number
, Duration(..), Location(..), Pitch(..) -- comparisons
, (@-@), (#-#), (~-~) -- difference operators
, sharp, flat -- change pitch
, raise, lower -- change location
, halve, double, dot, undot -- change duration
, midC -- whole note middle C
, whole, half, quarter, eighth -- common durations
) where

import Test.QuickCheck
import Control.Monad
import Data.Function (on)

data Letter = C | D | E | F | G | A | B deriving (Eq, Ord, Show, Enum, Bounded)
instance Arbitrary Letter where
  arbitrary = elements [ C .. B ]
safeSucc l | l == B = C
           | otherwise = succ l
safePred l | l == C = B
           | otherwise = pred l

succ' 0 l = l
succ' n l = succ' (n-1) (safeSucc l)

pred' 0 l = l
pred' n l = pred' (n-1) (safePred l)

type Octave = Integer
type Accidental = Integer
type Halves = Integer
type Dots = Integer

newtype Note = N (Letter, Octave, Accidental, Halves, Dots) deriving Eq
letter (N (x,_,_,_,_)) = x
octave (N (_,x,_,_,_)) = x
accidental (N (_,_,x,_,_)) = x
halves (N (_,_,_,x,_)) = x
dots (N (_,_,_,_,x)) = x
runN (N x) = x

makeNote l o a h d = N (l,o,a,h,d)

durAsNum n = (1/2)^^(halves n) * (3/2)^^(dots n)

instance Arbitrary Note where
  arbitrary = liftM5 makeNote arbitrary arbitrary arbitrary (arbitrary `suchThat` (>=0)) (arbitrary `suchThat` (>=0))

instance Show Note where
  show n = let (l,o,a,h,d) = runN n in show l ++ show o ++ (if a < 0 then show a else '+':show a) ++ '@':show h ++ 'h':show d ++ "d"

--- Duration comparison
newtype Duration = Duration { runDuration :: Note }
instance Show Duration where
  show (Duration n) = show (halves n) ++ 'h': show (dots n) ++ "d"
instance Eq Duration where
  (==) = (==) `on` (durAsNum . runDuration)
instance Ord Duration where
  compare = compare `on` (durAsNum . runDuration)

--- Location comparison
loc n = (octave n, letter n)
newtype Location = Location { runLocation :: Note }
instance Show Location where
  show (Location n) = show (letter n) ++ show (octave n)
instance Eq Location where
  (==) = (==) `on` (loc . runLocation)
instance Ord Location where
  compare = compare `on` (loc . runLocation)

-- Pitch comparison
normalizePitch n@(N (C,o,_,_,_)) 
  | o > 4 = normalizePitch (lower (7*(o-4)) n)
  | o < 4 = normalizePitch (raise (7*(4-o)) n)
  | otherwise = n
normalizePitch n@(N (l,_,_,_,_)) = normalizePitch (raise distToC n)
  where distToC = fromIntegral . length . takeWhile (/=C) . iterate (succ' 1) $ l
pitch = accidental . normalizePitch
newtype Pitch = Pitch { runPitch :: Note }
instance Show Pitch where
  show (Pitch n) = show (letter n) ++ show (octave n) ++ let a = accidental n in if a < 0 then show a else '+' : show a
instance Eq Pitch where
  (==) = (==) `on` (pitch . runPitch)
instance Ord Pitch where
  compare = compare `on` (pitch . runPitch)

--- Differences
a @-@ b = durAsNum a - durAsNum b
a #-# b = lines a - lines b
  where lines = linesFromMidC . loc
        linesFromMidC (o,l) = 7*(o-4) + (fromIntegral . fromEnum $ l)
a ~-~ b = pitch a - pitch b

sharp, flat, raise, lower, halve, double, dot, undot :: Integer -> Note -> Note

sharp i n = let (l,o,a,h,d) = runN n in N (l,o,a+i,h,d)
flat i n = sharp (negate i) n

raise i (N (l,o,a,h,d)) = N (succ' i l, o + octChange, a - offset, h, d)
  where notesToOctBorder = fromIntegral . length . takeWhile (/=B) . iterate (succ' 1) $ l -- num notes before octave number changes
        nonOctaveNotes = i `mod` 7 -- number of notes that are not part of straight octave increases
        numOctavesAdded = i `div` 7 -- number of octaves added
        octaveBorderCrossed = nonOctaveNotes > notesToOctBorder -- do we cross the octave border?
        oneSteps = fromIntegral . length . filter (`elem`[C,F]) . map (flip succ' l) $ [1..nonOctaveNotes] -- how many one steps in the non octave part?
        offset = oneSteps + 2*(nonOctaveNotes - oneSteps) + 12*numOctavesAdded -- the total offset
        octChange = numOctavesAdded + if octaveBorderCrossed then 1 else 0 -- total octave change
lower i (N (l,o,a,h,d)) = N (pred' i l, o - octChange, a + offset, h, d)
  where notesToOctBorder = fromIntegral . length . takeWhile (/=C) . iterate (pred' 1) $ l
        nonOctaveNotes = i `mod` 7
        numOctavesAdded = i `div` 7
        octaveBorderCrossed = nonOctaveNotes > notesToOctBorder
        oneSteps = fromIntegral . length . filter (`elem`[B,E]) . map (flip pred' l) $ [1..nonOctaveNotes]
        offset = oneSteps + 2*(nonOctaveNotes - oneSteps) + 12*numOctavesAdded
        octChange = numOctavesAdded + if octaveBorderCrossed then 1 else 0

halve i n = let (l,o,a,h,d) = runN n in N (l,o,a,h+i,d)
double i n = halve (negate i) n

dot i n = let (l,o,a,h,d) = runN n in N (l,o,a,h,d+i)
undot i n = dot (negate i) n

midC = N (C,4,0,0,0)

-- durations
whole = N (C,4,0,0,0)
half  = N (C,4,0,1,0)
quarter = N (C,4,0,2,0)
eighth = N (C,4,0,3,0)


test = do
  quickCheck $ \n (NonNegative i) -> n == (sharp i . flat i   $ n)
  quickCheck $ \n (NonNegative i) -> n == (raise i . lower i  $ n)
  quickCheck $ \n (NonNegative i) -> n == (halve i . double i $ n)
  quickCheck $ \n (NonNegative i) -> n == (dot i   . undot i  $ n)

  quickCheck $ \n (Positive i) -> Duration n > Duration (halve i n)
  quickCheck $ \n (Positive i) -> Duration n < Duration (dot i n)
  quickCheck $ \n (Positive i) -> Duration n < Duration (double i n)
  quickCheck $ \n (Positive i) -> Duration n > Duration (undot i n)
  quickCheck $ \n (Positive i) -> Location n < Location (raise i n)
  quickCheck $ \n (Positive i) -> Location n > Location (lower i n)
  quickCheck $ \n (Positive i) -> Pitch n < Pitch (sharp i n)
  quickCheck $ \n (Positive i) -> Pitch n > Pitch (flat i n)
  
  quickCheck $ \n (Positive i) -> i == ((sharp i n) ~-~ n)
  quickCheck $ \n (Positive i) -> i == ((raise i n) #-# n)
