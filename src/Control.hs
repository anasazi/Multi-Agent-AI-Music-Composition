module Control
( makeControl, controlLoop
, ControlContext(..)
, BlackboardContext(..)
, ViolationVec(..)
, bestBlackboard
) where

import Blackboard
import Agent

import Note
import Voice
import Scale

import qualified Data.List as L
import Data.Function (on)
import Control.Applicative
import System.Random
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Monoid

import Debug.Trace (trace)

type Time = Double

data TestLoc = TL Time Agent deriving Show
testTime (TL x _) = x
testAgent (TL _ x) = x

{- 
The main goal is to gain the ability to track children/parents.
Since data is immutable, if we kept pointers to their contexts, we would not see test updates.
Thus, move metadata storage into a Map from Blackboards to metadata.
Now we can add children Blackboards as metadata.
Since only the counterpoint is used to determine eq/ord of blackboards, we're fine.

Then we can generate the vectors of rule violations for descendents and use it to order.
-}
type Violation = Integer
data BlackboardContext = BlackboardContext
  { hard :: ViolationVec
  , soft :: ViolationVec
  , toRun :: [ TestLoc ]
  , parent :: Maybe Blackboard
  } deriving (Show)
type BlackboardMap = M.Map Blackboard BlackboardContext

-- base context
emptyBBCxt :: BlackboardContext
emptyBBCxt = BlackboardContext (VV [0]) (VV [0]) [] Nothing

-- create the map with a base blackboard
startWith :: Blackboard -> BlackboardMap
startWith b = M.singleton b emptyBBCxt

-- change the hard vector
addHard :: ViolationVec -> Blackboard -> BlackboardMap -> BlackboardMap
addHard v b m = continue . M.adjust (\cxt -> cxt { hard = new }) b $! m
  where cxt = m M.! b
        orig = hard cxt
        new@(VV newls) = v `mappend` orig
        continue = case parent cxt of
                    Nothing -> id
                    (Just p) -> addHard (VV $! 0:newls) p

-- change the soft vector
addSoft :: ViolationVec -> Blackboard -> BlackboardMap -> BlackboardMap
addSoft v b m = continue . M.adjust (\cxt -> cxt { soft = new}) b $! m
  where cxt = m M.! b
        orig = soft cxt
        new@(VV newls) = v `mappend` orig
        continue = case parent cxt of
                    Nothing -> id
                    (Just p) -> addSoft (VV $! 0:newls) p

-- add a blackboard. cascade the change upwards
type Parent = Blackboard
type Child = Blackboard
addChild :: Parent -> Child -> BlackboardMap -> BlackboardMap
addChild p c m = addHard mempty c . addSoft mempty c . M.insert c (emptyBBCxt { parent = Just p }) $! m

-- get the best blackboard
bestBlackboard :: BlackboardMap -> Blackboard
bestBlackboard m = fst . L.maximumBy (comparing snd) $! merged
  where getAll = M.assocs . M.mapWithKey (\k v -> (hard v, soft v, durationOfCounterPoint k)) $! m
        merged = map (\(b,(h,s,l)) -> (b, (mergeVV h s, l))) getAll

-- Define the ordering of two violation vectors.
-- We want less violations, so smaller numbers are better.
-- If we run off one list, we consider it to be extended with 0's.
newtype ViolationVec = VV [Violation] deriving (Eq, Show)
instance Ord ViolationVec where
  compare (VV []) (VV []) = EQ
  compare (VV []) (VV (b:bs)) | b == 0 = comparing VV [] bs
                              | otherwise = GT
  compare (VV (a:as)) (VV []) | a == 0 = comparing VV as []
                              | otherwise = LT
  compare (VV (a:as)) (VV (b:bs)) | a == b = comparing VV as bs
                                  | otherwise = comparing negate a b
instance Monoid ViolationVec where
  mempty = VV []
  mappend (VV []) b = b
  mappend a (VV []) = a
  mappend (VV (a:as)) (VV (b:bs)) = let (VV v) = mappend (VV as) (VV bs) in VV $! a+b : v
                                  
-- Given two ViolationVecs of the same length, create one that alternates elements from each
mergeVV :: ViolationVec -> ViolationVec -> ViolationVec
mergeVV (VV a) (VV b) = VV . concat . zipWith (\x y -> [x,y]) a $! b

-- is it long enough?
longEnough :: Blackboard -> Bool
longEnough b = durationOfCounterPoint b == durationOfCantusFirmus b

-- are there more tests?
outOfTests :: Blackboard -> BlackboardMap -> Bool
outOfTests b bm = null . toRun . (M.! b) $ bm

-- run the next test
{-applyTest :: Blackboard -> BlackboardMap -> BlackboardMap
applyTest b bm = M.adjust (\bc -> bc { hard = newHard, soft = newSoft, toRun = tests }) b bm
  where cxt = bm M.! b
        (tl:tests) = toRun cxt
        agent = testAgent tl
        result = operate agent (lookAt b (testTime tl))
        newHard = hard cxt + if isHardRule agent && not (testResult result) then 1 else 0
        newSoft = soft cxt + if isSoftRule agent && not (testResult result) then 1 else 0 -}

-- run the queued tests
applyAllTests :: Blackboard -> BlackboardMap -> BlackboardMap
applyAllTests b m = addHard hardViolations b . addSoft softViolations b . clearTests b $! m
  where cxt = m M.! b
        (softTLs, hardTLs) = L.partition (isSoftRule . testAgent) (toRun cxt)
        testAndCount = L.genericLength . filter (not . testResult) 
                        . map (\tl -> operate (testAgent tl) (lookAt b (testTime tl))) 
        softViolations = VV . (:[]) . testAndCount $! softTLs
        hardViolations = VV . (:[]) . testAndCount $! hardTLs 

-- queue up more tests
addTests :: Blackboard -> [TestLoc] -> BlackboardMap -> BlackboardMap
addTests b tests = M.adjust (\cxt -> cxt { toRun = toRun cxt ++ tests }) b

-- nuke tests
clearTests :: Blackboard -> BlackboardMap -> BlackboardMap
clearTests b = M.adjust (\cxt -> cxt { toRun = [] }) b

data ControlContext = ControlContext
  { testers :: [ Agent ]
  , generators :: [ Agent ]
  , targetDuration :: Time
  , blackboards :: BlackboardMap
  , randGen :: StdGen
  } deriving Show

makeControl :: [Agent] -> Scale -> Voice -> StdGen -> ControlContext
makeControl agents scale source rg = ControlContext testers generators targetDur blackboards rg
  where testers = filter isTester agents
        generators = filter isGenerator agents
        targetDur = durationOfVoice source
        basedscale = scale .$ (head . focus . front $ source)
        blackboards = startWith (create source basedscale rg)

controlLoop :: ControlContext -> ControlContext
controlLoop cc | longEnough best && outOfTests best bm = cc
               | outOfTests best bm = controlLoop (applyGen cc' genAgent)
               | otherwise = controlLoop $! cc { blackboards = applyAllTests best bm }
  where bm = blackboards cc
        best = bestBlackboard bm
        (rint, newGen) = randomR (0, length (generators cc) - 1) (randGen cc)
        genAgent = generators cc !! rint
        cc' = cc { randGen = newGen }

-- apply a generator to a blackboard and put the new blackboard in the map
applyGen :: ControlContext -> Agent -> ControlContext
applyGen cc gen = cc { blackboards = modded, randGen = rGen newBoard }
  where bm = blackboards cc
        best = bestBlackboard bm
        newBoard = operate gen (setGen best (randGen cc))
        changedTimes = findChangesInCP best newBoard
        tests = TL <$> changedTimes <*> testers cc
        modded = trace (show . focus . front . counterPoint $! newBoard)
                  addTests newBoard tests . addChild best newBoard $! bm

-- This is ok as is
findChangesInCP :: Blackboard -> Blackboard -> [Time]
findChangesInCP = findChangesInVoice `on` counterPoint

findChangesInVoice :: Voice -> Voice -> [Time]
findChangesInVoice old new = let oldNotes = focus (front old)
                                 newNotes = focus (front new)
                                 oldTimes = scanl (+) 0 $ map durAsNum oldNotes
                                 newTimes = scanl (+) 0 $ map durAsNum newNotes
                                 old' = zip oldTimes oldNotes
                                 new' = zip newTimes newNotes
                                 eq (t,n) (s,m) = t == s && n == m && 
                                                  Pitch n == Pitch m &&
                                                  Location n == Location m
                                 changed = L.deleteFirstsBy eq new' old'
                             in map fst changed
