
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
import Control.Monad
import Note

class Interval a where
	size :: a -> Integer -- the difference in note letter names
	quality :: a -> Quality -- the quality of the interval (useful for analysis)
	width :: a -> Integer -- the difference in half steps
	invert :: a -> a -- gives the inversion of the interval

data Quality = Diminished | Minor | Perfect | Major | Augmented
	deriving (Eq, Ord, Read, Show, Enum, Bounded)
data Size = S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8
	deriving (Eq, Ord, Read, Show, Enum, Bounded)

data SimpleInterval = SI Size Quality
	deriving (Eq, Read, Show, Ord)

instance Arbitrary SimpleInterval where
	arbitrary = elements allSimpleIntervals

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

sizeInversions = zip [S1 .. S8] [S8, S7 .. S1]
qualityInversions = zip [Diminished .. Augmented] [Augmented, Major .. Diminished]

lookupUnsafe x = snd . head . dropWhile ((/=x) . fst)

instance Interval SimpleInterval where
	size (SI s _) = 1 + (toInteger . fromEnum) s
	quality (SI _ q) = q
	width i = lookupUnsafe i allIntervalWidths
	invert (SI s q) = SI (lookupUnsafe s sizeInversions) (lookupUnsafe q qualityInversions)

data CompoundInterval = CI SimpleInterval Integer
	deriving (Eq, Read, Show, Ord)

instance Interval CompoundInterval where
	size (CI si _) = 7 + size si
	quality (CI si _) = quality si
	width (CI si octs) = 12 * octs + width si
	invert (CI si octs) = CI (invert si) octs

instance Arbitrary CompoundInterval where
	arbitrary = suchThat (liftM2 CI arbitrary (suchThat arbitrary (\x -> x>0 && x<5))) (\x -> x /= (CI unison 1) && x /= (CI (SI S2 Diminished) 1))

unison	= SI S1 Perfect
octave	= SI S8 Perfect
maj3		= SI S3 Major
perf5		= SI S5 Perfect

isPerfect, isMajor, isMinor, isAugmented, isDiminished :: Interval a => a -> Bool
isPerfect			= (==Perfect)			. quality
isMajor				= (==Major)				. quality 
isMinor				= (==Minor)				. quality 
isAugmented		= (==Augmented)		. quality 
isDiminished	= (==Diminished)	. quality 

isSimple, isCompound :: Interval a => a -> Bool
isSimple = (<=12) . width
isCompound = not . isSimple

-- TODO
halfStepDistance :: Note -> Note -> Integer
halfStepDistance = undefined

-- TODO - test that generated compounds are actually compound (not unisons)
getInterval :: Interval a => Note -> Note -> a
getInterval = undefined

applyIntervalUp :: Interval a => a -> Note -> Note
applyIntervalUp i n = nStepUp (fromInteger . width $ i) n -- foldl (flip ($)) n (replicate (fromInteger . width $ i) halfStepUp)

applyIntervalDown :: Interval a => a -> Note -> Note
applyIntervalDown i n = nStepDown (fromInteger . width $ i) n --foldl (flip ($)) n (replicate (fromInteger . width $ i) halfStepDown)

-- tests
prop_applyIntervalUpDownSimple' :: SimpleInterval -> Note -> Bool
prop_applyIntervalUpDownSimple' i n = n == (applyIntervalDown i . applyIntervalUp i $ n)

prop_applyIntervalUpDownSimple i n = ((nStepDown (fromInteger . width $ i) maxBound) >= n) ==> (prop_applyIntervalUpDownSimple' i n)

prop_applyIntervalUpDownCompound' :: CompoundInterval -> Note -> Bool
prop_applyIntervalUpDownCompound' i n = n == (applyIntervalDown i . applyIntervalUp i $ n)

prop_applyIntervalUpDownCompound i n = ((nStepDown (fromInteger . width $ i) maxBound) >= n) ==> (prop_applyIntervalUpDownCompound' i n)

prop_invertInverse i = i == (invert . invert $ i)

prop_isSimpleOnSimple :: SimpleInterval -> Bool
prop_isSimpleOnSimple i = isSimple i == True

prop_isSimpleOnCompound :: CompoundInterval -> Bool
prop_isSimpleOnCompound i = isSimple i == False

prop_positiveWidth i = width i >= 0

test = do
	quickCheck (prop_invertInverse :: SimpleInterval -> Bool)
	quickCheck (prop_invertInverse :: CompoundInterval -> Bool)
	quickCheck prop_isSimpleOnSimple
	quickCheck prop_isSimpleOnCompound
	quickCheck (prop_positiveWidth :: SimpleInterval -> Bool)
	quickCheck (prop_positiveWidth :: CompoundInterval -> Bool)
	quickCheck prop_applyIntervalUpDownSimple
	quickCheck prop_applyIntervalUpDownCompound
