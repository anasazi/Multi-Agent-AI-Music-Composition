module Interval
(
) where

import qualified Pitch as P

import Test.QuickCheck
import Control.Monad
import Control.Arrow
import Data.Function (on)

data Interval = I { lspan :: Integer, width :: Integer }
	deriving  (Eq, Ord --) -- notational equality and ordering
							, Show) -- TODO temp, eventually will display in usual format

instance Arbitrary Interval where
	arbitrary = liftM2 I (arbitrary `suchThat` (>=1)) (arbitrary `suchThat` (>=0))

-- quality values
data Quality = Dim Integer | Minor | Perfect | Major | Aug Integer
	deriving Eq
-- TODO quality is technically a poset, how do i implement that

-- accessor
quality :: Interval -> Quality
quality i | perfectSpec i = undefined
					| minmajSpec i	= undefined

-- predicates
simple, compound :: Interval -> Bool
simple = lspan &&& width >>> (<=8) *** (<=12) >>> uncurry (&&)
compound = not . simple

perfectSpec, minmajSpec :: Interval -> Bool
perfectSpec = (`elem`[1,4,5]) . (`mod`7) . lspan
minmajSpec = (`elem`[2,3,6,0]) . (`mod`7) . lspan

-- unary operators
invert :: Interval -> Interval
invert i 
	| simple i = (lspan &&& width >>> (9-) *** (12-) >>> uncurry I) i
	| otherwise = invert (simplify i)

simplify :: Interval -> Interval
simplify i 
	| simple i = i
	| otherwise = (lspan &&& width >>> (+1) . (`mod`7) . subtract 1 *** (`mod`12) >>> uncurry I) i

-- binary operators
	-- compose
(##) :: Interval -> Interval -> Interval
a ## b = I (lspan a + lspan b - 1) (width a + width b)

(#^), (#.) :: Interval -> P.Pitch -> P.Pitch
	-- apply up
a #^ b = (lspan &&& width >>> fromInteger . subtract 1 *** fromInteger >>> flip (!!) *** flip (!!) >>> ($(iterate (P.up.) id)) *** ($(iterate (P.sharp.) id)) >>> uncurry (.) $ a) b
	-- apply down
a #. b = (lspan &&& width >>> fromInteger . subtract 1 *** fromInteger >>> flip (!!) *** flip (!!) >>> ($(iterate (P.down.) id)) *** ($(iterate (P.flat.) id)) >>> uncurry (.) $ a) b

-- TODO slow on *very* large intervals (thus not very important)
(#) :: P.Pitch -> P.Pitch -> Interval
	-- build
a # b = I lspan width
	where
	-- harmonic ordering
	low		| a <= b		= a | otherwise = b
	high	| low == a	= b | otherwise = a
	-- spatial ordering
	bot		| ((<=) `on` P.point) a b = a | otherwise = b
	top		| bot == a								= b | otherwise = a
	-- find the letter span
	lspan = fromIntegral $ 1 + length (takeWhile (((/=) `on` P.point) top) $ iterate P.up bot)
	-- find the width
	width = fromIntegral $ length (takeWhile ((/=) high) $ iterate P.sharp low)

-- tests
qc :: Testable prop => prop -> IO ()
qc = quickCheckWith stdArgs { maxSuccess = 500, maxDiscard = 2500 }

swap (a,b) = (b,a)

valid = lspan &&& width >>> (>0) *** (>=0) >>> uncurry (&&)

test = do
	P.test
	putStrLn "Interval.hs tests"
	qc $ uncurry (/=) <<< perfectSpec &&& minmajSpec -- every interval is on exactly one spectrum
	qc $ uncurry (==) <<< invert . invert &&& id <<< simplify -- invert is inverse on simples
	qc $ simple . invert -- all inverses are simple
	qc $ simple . simplify -- simplify only produces simple intervals
	qc $ first ( (#.) &&& (#^) >>> uncurry (.) ) >>> uncurry ($) &&& snd >>> uncurry (==) -- changing the direction of application is the inverse
	qc $ first ( (#.) *** (#^) >>> uncurry (.) &&& uncurry (.) . swap ) >>> (\((a,a'),b) -> (a b, a' b)) >>> uncurry (==) -- different direction application is commutative
	qc $ first ( (#.) *** (#.) >>> uncurry (.) &&& uncurry (.) . swap ) >>> (\((a,a'),b) -> (a b, a' b)) >>> uncurry (==) -- application down is commutative
	qc $ first ( (#^) *** (#^) >>> uncurry (.) &&& uncurry (.) . swap ) >>> (\((a,a'),b) -> (a b, a' b)) >>> uncurry (==) -- application up is commutative
	qc $ first ( uncurry (##) &&& (swap >>> uncurry (##)) ) >>> (\((a,a'),b) -> (a #^ b, a'#^ b)) >>> uncurry (==) -- composition is commutative
	qc $ valid
	qc $ valid . invert
	qc $ valid . simplify
	qc $ valid . uncurry (##)
	quickCheckWith stdArgs { maxSuccess = 50 } $ valid . uncurry (#) -- takes a long time because the tester generates notes very far apart so we need to build *very* large intervals (on the order of 100s).
