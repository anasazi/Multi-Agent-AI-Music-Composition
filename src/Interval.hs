
{-
The Interval module provides an ADT representing musical intervals, which are the distances between two notes.

Intervals have two essential characteristics: size and quality.
Size is the difference between the note letter names.
Quality combined with size specifies the precise number of half steps between the notes.

Intervals not larger than an octave are called simple.
Intervals larger than an octave are called compound and can be analyzed as simple intervals by subtracting 8ves as necessary.
	This means that compound intervals can be specified as a simple interval and a number of 8ves.
-}

import Test.QuickCheck

class Interval a where
	size :: a -> Integer -- the difference in note letter names
	quality :: a -> Quality -- the quality of the interval (useful for analysis)
	width :: a -> Integer -- the difference in half steps

data Quality = Diminished | Minor | Perfect | Major | Augmented
	deriving (Eq, Ord, Read, Show, Enum, Bounded)
data Size = S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8
	deriving (Eq, Ord, Read, Show, Enum, Bounded)

data SimpleInterval = SI Size Quality
	deriving (Eq, Read, Show, Ord)

isValid (SI s q)
	| s `elem` [S2,S3,S6,S7] = q `elem` [Diminished,Minor,Major,Augmented]
	| s `elem` [S4,S5] = q `elem` [Diminished,Perfect,Augmented]
	| s == S1 = q `elem` [Perfect,Augmented]
	| s == S8 = q `elem` [Diminished,Perfect]

allSimpleIntervals = filter isValid [ SI s q | s <- [S1 .. S8], q <- [Diminished .. Augmented] ]

allMainIntervals = filter ((`elem` [Minor,Major,Perfect]) . quality) allSimpleIntervals

allIntervalWidths = filter (\(_,n) -> n >= 0 && n <= 12) $ concatMap f (zip allMainIntervals $ [0..5]++[7..12])
	where
	f i@((SI s Major,n)) = [i, (SI s Augmented, n+1)]
	f i@((SI s Minor,n)) = [(SI s Diminished, n-1),i]
	f i@((SI s Perfect,n)) = [(SI s Diminished, n-1), i, (SI s Augmented, n+1)]

instance Interval SimpleInterval where
	size (SI s _) = 1 + (toInteger . fromEnum) s
	quality (SI _ q) = q
	width i = snd . head . dropWhile ((/=i) . fst) $ allIntervalWidths

data CompoundInterval = CI SimpleInterval Integer
	deriving (Eq, Read, Show, Ord)

instance Interval CompoundInterval where
	size (CI si _) = 7 + size si
	quality (CI si _) = quality si
	width (CI si octs) = 12 * octs + width si

{- TODO
function that given two notes returns the interval
function that given a base note and an interval returns the other note
function that inverts an interval
functions for common or special intervals
-}
