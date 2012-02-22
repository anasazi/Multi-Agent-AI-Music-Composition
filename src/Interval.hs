module Interval
( Quality(..)
, Size
, HalfStep
, Interval
, size
, width
, quality
, invert
, (#^)
, (#.)
, (#)
, ($=$) , ($>$) , ($<$)
, (\=/) , (\>/) , (\</)
, unison, min2, maj2, min3, maj3, perf4, aug4, dim5, perf5, min6, maj6, min7, maj7, octave
) where

import Test.QuickCheck
import Control.Monad
import Note
import Maybe (fromJust)

data Quality = Dim | Min | Perf | Maj | Aug deriving (Eq, Ord, {-Show,-} Enum)
instance Show Quality where
	show Dim = "o"
	show Min = "m"
	show Perf = "P"
	show Maj = "M"
	show Aug = "+"
instance Arbitrary Quality where
	arbitrary = elements [ Dim .. Aug ]
invQ :: Quality -> Quality
invQ = toEnum . (4-) . fromEnum

-- Number of letters from bottom to top inclusive (a unison is 1 letter span).
data LetterSpan = L1 | L2 | L3 | L4 | L5 | L6 | L7 | L8
	deriving (Eq, Ord, {-Show,-} Enum)
instance Show LetterSpan where
	show = show . (+1) . fromEnum
instance Arbitrary LetterSpan where
	arbitrary = elements [ L1 .. ]
invLS :: LetterSpan -> LetterSpan
invLS = fromJust . (flip lookup $ zip [L1,L2,L3,L4,L5,L6,L7,L8] [L8,L7,L6,L5,L4,L3,L2,L1])

type Size = Integer
type HalfStep = Integer

-- In this scheme an octave is denoted as C (S L1 Perf)
data Interval = S LetterSpan Quality -- A interval less than an octave
							| C Interval -- An interval extended by an octave
		deriving (Eq{-, Show-})
instance Show Interval where
	show (S ls q) = show q ++ show ls
	show (C i) = "*" ++ show i--show (quality i) ++ show (7 + size i) ++ "(*" ++ show i ++ ")"
instance Arbitrary Interval where
	arbitrary = oneof [ liftM2 S arbitrary arbitrary `suchThat` isValid
										, liftM C (arbitrary `suchThat` (\i -> size i > 1))
										]
		where isValid (S ls q)
						| ls == L1 = q `elem` [Perf, Aug]
						| ls `elem` [L4,L5,L8] = q `elem` [Dim,Perf,Aug]
						| otherwise = q `elem` [Dim,Min,Maj,Aug]

isSimple (S _ _) = True
isSimple _ = False
-- Return the size (letter span) as an number
size :: Interval -> Size
size (S ls _) = (1+) . fromIntegral . fromEnum $ ls
size (C i)		= (7+) . size $ i
-- Return the width (half steps)
width :: Interval -> HalfStep
width (S ls q) = base + mod
	where
	base = fromJust . lookup ls $ zip [ L1 .. ] [ 0,2,4,5,7,9,11,12 ]
	mod
		| ls == L1							= case q of Perf -> 0; Aug -> 1
		| ls == L8							= case q of Dim -> (-1); Perf -> 0; Aug -> 1
		| ls `elem` [L4,L5]			= case q of Dim -> (-1); Perf -> 0; Aug -> 1
		| otherwise							= case q of Dim -> (-2); Min -> (-1); Maj -> 0; Aug -> 1
width (C i) = (12+) . width $ i
-- Return the quality
quality :: Interval -> Quality
quality (S _ q) = q
quality (C i) = quality i
-- Generate the inversion of the interval
invert :: Interval -> Interval
invert (S ls q) = S (invLS ls) (invQ q)
invert (C i) = invert i
-- Apply the interval upwards
(#^) :: Interval -> Note -> Note
i@(S _ _) #^ n = raisePitch . raiseLetter $ n
	where
	raiseLetter = foldl (.) id (replicate (fromInteger . subtract 1 . size $ i) up)
	raisePitch = foldl (.) id (replicate (fromInteger . width $ i) sharp)
i@(C si) #^ n = raiseOctave (si #^ n)
	where raiseOctave = foldl (.) id (replicate 12 sharp) . foldl (.) id (replicate 7 up)
-- Apply the interval downwards
(#.) :: Interval -> Note -> Note
i@(S _ _) #. n = lowerPitch . lowerLetter $ n
	where
	lowerLetter = foldl (.) id (replicate (fromInteger . subtract 1 . size $ i) down)
	lowerPitch = foldl (.) id (replicate (fromInteger . width $ i) flat)
i@(C si) #. n = lowerOctave (si #. n)
	where lowerOctave = foldl (.) id (replicate 12 flat) . foldl (.) id (replicate 7 down)
-- Make an interval from two notes
--(#) :: Note -> Note -> Interval
a # b | size <= 8 = S ls q | otherwise = C ((octave #^ bot) # top)
	where
	-- positional ordering
	bot | a -<- b = a | otherwise = b
	top | bot == a = b | otherwise = a
	-- acoustical ordering
	low | a =<= b = a | otherwise = b
	high | low == a = b | otherwise = a
	-- find the letter span by stepping up until location equality
	size | a -=- b = 0
			 | otherwise = length . takeWhile (not . (-=-top)) $ iterate up bot
	ls = toEnum (size `mod` 8) :: LetterSpan
	-- find the width by sharping until enharmonic equality
	width = length . takeWhile (not . (===high)) $ iterate sharp low
	width' = width `mod` 12
	-- derive quality form size and width
	base = fromJust $ lookup ls $ zip [ L1 .. ] [ 0,2,4,5,6,9,11,0 ]
	q | ls == L1					= case width' - base of 0 -> Perf; 1 -> Aug
		| ls == L8					= case width' - base of 11 -> Dim; 0 -> Perf; 1 -> Aug--(-1) -> Dim; 0 -> Perf
		| ls `elem` [L4,L5]	= case width' - base of (-1) -> Dim; 0 -> Perf; 1 -> Aug
		| otherwise					= case width' - base of (-2) -> Dim; (-1) -> Min; 0 -> Maj; 1 -> Aug

-- Size equality and ordering
a $=$ b = size a == size b
a $>$ b = size a >  size b
a $<$ b = size a <  size b

-- Width equality and ordering
a \=/ b = width a == width b
a \>/ b = width a >  width b
a \</ b = width a <  width b

----- COMMON INTERVALS -----
unison = S L1 Perf
min2 = S L2 Min
maj2 = S L2 Maj
min3 = S L3 Min
maj3 = S L3 Maj
perf4 = S L4 Perf
aug4 = S L4 Aug
dim5 = S L5 Dim
perf5 = S L5 Perf
min6 = S L6 Min
maj6 = S L6 Maj
min7 = S L7 Min
maj7 = S L7 Maj
octave = S L8 Perf



f :: Testable prop => prop -> IO ()
f = quickCheckWith stdArgs { maxSuccess = 1000, maxDiscard = 5000 }

test = do
	f $ \q -> (==q) . invQ . invQ $ q
	f $ \ld -> (==ld) . invLS . invLS $ ld
	f $ \i -> isSimple i ==> (==i) . invert . invert $ i
	f $ \i -> isSimple . invert $ i
	f $ \i -> (/=i) . invert $ i
	f $ \i -> (>=0) . width $ i
	f $ \i n -> (==n) . (i#.) . (i#^) $ n
	f $ \i n -> (==n) . (i#^) . (i#.) $ n
	f $ \i n -> (<=4) . accidentals . (i#^) $ n
	f $ \i n -> (>=(-4)) . accidentals . (i#^) $ n
	f $ \i n -> (<=4) . accidentals . (i#.) $ n
	f $ \i n -> (>=(-4)) . accidentals . (i#.) $ n
	f $ \i -> (>0) . size $ i
	f $ \i -> isSimple i ==> (<9) . size $ i
	f $ \i -> not (isSimple i) ==> let (C i') = i in size i' > 1

