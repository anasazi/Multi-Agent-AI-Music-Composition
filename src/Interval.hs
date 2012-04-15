module Interval
( LetterSpan, HalfStepWidth
, Interval, lspan, width, runI, makeInterval
, Span(..), Width(..), Quality(..)
, quality
, simple, compound
, isPerfectLetterSpan, isMinorMajorLetterSpan
, invert
, simplify
, shorten, lengthen
, shrink, grow
, compose, (##)
, apUp, (#^)
, apDn, (#.)
, build, (#)
, unison, min2, maj2, min3, maj3, perf4, perf5, min6, maj6, min7, maj7, octave
) where

import Note
import Test.QuickCheck hiding (shrink)
import Control.Monad
import Data.Function (on)

type LetterSpan = Integer
type HalfStepWidth = Integer
newtype Interval = I (LetterSpan, HalfStepWidth) deriving Eq
lspan (I (x,_)) = x
width (I (_,x)) = x
runI (I x) = x

makeInterval ls w = I (ls,w)

-- common intervals
i = curry I
unison = i 1 0
min2   = i 2 1
maj2   = i 2 2
min3   = i 3 3
maj3   = i 3 4
perf4  = i 4 5
perf5  = i 5 7
min6   = i 6 8
maj6   = i 6 9
min7   = i 7 10
maj7   = i 7 11
octave = i 8 12

instance Show Interval where
  show i = show (quality i) ++ show (lspan i)

instance Arbitrary Interval where
  arbitrary = liftM2 makeInterval (arbitrary `suchThat` (>=1)) (arbitrary `suchThat` (>=0))

-- Interval comparisons
newtype Span = Span { runSpan :: Interval }
instance Eq Span where
  (==) = (==) `on` (lspan . runSpan)
instance Ord Span where
  compare = compare `on` (lspan . runSpan)
instance Show Span where
  show = show . lspan . runSpan

newtype Width = Width { runWidth :: Interval }
instance Eq Width where
  (==) = (==) `on` (width . runWidth)
instance Ord Width where
  compare = compare `on` (width . runWidth)
instance Show Width where
  show = show . width . runWidth

-- Technically there isn't an ordering between Minor/Major and Perfect
data Quality = Dim Integer | Minor | Perfect | Major | Aug Integer deriving Eq

instance Ord Quality where
  compare (Dim a) (Dim b) = compare (negate a) (negate b)
  compare (Dim _) _       = LT
  compare _       (Dim _) = GT

  compare Minor   Minor   = EQ
  compare Minor   _       = LT
  compare _       Minor   = GT

  compare Perfect Perfect = EQ
  compare Perfect _       = LT
  compare _       Perfect = GT

  compare Major   Major   = EQ
  compare Major   _       = LT
  compare _       Major   = GT

  compare (Aug a) (Aug b) = compare a b

instance Show Quality where
  show Minor = "m"
  show Perfect = "P"
  show Major = "M"
  show (Dim n) = show n ++ "o"
  show (Aug n) = show n ++ "+"

-- Determine the quality of an interval
quality :: Interval -> Quality
quality i
  | compound i                = quality (simplify i)
  | isPerfectLetterSpan i     = case diff of 0          -> Perfect
                                             n | n < 0  -> Dim (negate n)
                                               | n > 0  -> Aug n
  | isMinorMajorLetterSpan i  = case diff of 0          -> Major
                                             (-1)       -> Minor
                                             n | n > 0  -> Aug n
                                               | n < 0  -> Dim (negate n - 1)
  where diff = width i - case lspan i of 1->0; 2->2; 3->4; 4->5; 5->7; 6->9; 7->11; 8->12

-- test whether an interval is simple (octave or smaller) or compound
simple, compound :: Interval -> Bool
simple int = let (ls,w) = runI int in ls <= 8 && w <= 12
compound = not . simple

-- test whehter an interval is the Perfect spectrum or the Minor/Major spectrum
isPerfectLetterSpan = (`elem`[1,4,5]) . (`mod`7) . lspan
isMinorMajorLetterSpan = not . isPerfectLetterSpan

-- inverse of an interval
invert :: Interval -> Interval
invert i
  | simple i  = let (s,w) = runI i in I ( 9-s, 12-w )
  | otherwise = invert (simplify i)

-- get the simple interval underlying a given interval
simplify :: Interval -> Interval
simplify i 
  | simple i  = i
  | otherwise = let (s,w) = runI i in I (((s-1)`mod`7)+1, w`mod`12)


-- modify the width of an interval
shorten, lengthen :: Integer -> Interval -> Interval
shorten n i = I (lspan i, negate n + width i)
lengthen n i = I (lspan i, n + width i)

-- modify the letter span of an interval
shrink, grow :: Integer -> Interval -> Interval
shrink n i = I (negate n + lspan i, width i)
grow n i = I (n + lspan i, width i)

-- combine two intervals
compose :: Interval -> Interval -> Interval
compose a b = I ((lspan a + lspan b - 1), (width a + width b))
(##) = compose

-- apply intervals to notes
apUp, apDn :: Interval -> Note -> Note
apUp i n = let (s,w) = runI i in raise (s-1) . sharp w $ n
(#^) = apUp
apDn i n = let (s,w) = runI i in lower (s-1) . flat w $ n
(#.) = apDn

-- create an interval from two notes
build :: Note -> Note -> Interval
build a b = let l = a #-# b
                hs = a ~-~ b
            in I (abs l + 1, abs hs)
(#) = build

valid i = let (s,w) = runI i in s > 0 && w >= 0

test = do
  quickCheck $ \i -> isPerfectLetterSpan i /= isMinorMajorLetterSpan i
  quickCheck $ \i -> let i' = simplify i in i' == invert (invert i')
  quickCheck $ simple . invert
  quickCheck $ simple . simplify
  quickCheck $ \i -> simple i /= compound i
  quickCheck $ \i n -> n == (i #^ (i #. n))
  quickCheck $ \i i' n -> (i #^ (i' #. n)) == (i' #. (i #^ n))
  quickCheck $ \i i' n -> (i #^ (i' #^ n)) == (i' #^ (i #^ n))
  quickCheck $ \i i' n -> (i #. (i' #. n)) == (i' #. (i #. n))
  quickCheck $ \i i' n -> (i ## i') #^ n == (i' ## i) #^ n
  quickCheck $ \n n' -> (n # n') == (n' # n)
  quickCheck $ valid
  quickCheck $ valid . invert
  quickCheck $ valid . simplify
  quickCheck $ \a b -> valid (a ## b)
  quickCheck $ \a b -> valid (a # b)
  quickCheck $ \i (Positive n) -> i == (shorten n (lengthen n i))
  quickCheck $ \i (Positive n) -> i == (shrink n (grow n i))
  quickCheck $ \i (Positive n) -> Width i < Width (lengthen n i)
  quickCheck $ \i (Positive n) -> Span i < Span (grow n i)
  quickCheck $ \i (Positive n) -> let i' = lengthen (n+1) i in Width i' > Width (shorten n i')
  quickCheck $ \i (Positive n) -> let i' = grow (n+1) i in Span i' > Span (shrink n i')
  quickCheck $ \i (Positive a) (Positive b) -> lengthen a (grow b i) == grow b (lengthen a i)
