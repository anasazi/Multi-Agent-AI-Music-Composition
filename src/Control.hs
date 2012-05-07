module Control
( makeControl, controlLoop
, ControlContext(..)
, BlackboardContext(..)
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
type Child = Blackboard
data BlackboardContext = BlackboardContext
  { hard :: Violation
  , soft :: Violation
  , toRun :: [ TestLoc ]
  , children :: [ Child ]
  } deriving (Show)
type BlackboardMap = M.Map Blackboard BlackboardContext
instance Eq BlackboardContext where
  a == b = hard a == hard b && soft a == soft b && children a == children b

-- base context
emptyBBCxt :: BlackboardContext
emptyBBCxt = BlackboardContext 0 0 [] []

-- insert a child and add the child to its parent's children list
insertWithParent :: Blackboard -> Blackboard -> BlackboardMap -> BlackboardMap
insertWithParent p c bm = M.insert c emptyBBCxt . M.adjust addChild p $ bm
  where addChild bc = bc { children = c : children bc }

-- create the map with a base blackboard
startWith :: Blackboard -> BlackboardMap
startWith b = M.singleton b emptyBBCxt

-- get the best blackboard
bestBlackboard :: BlackboardMap -> Blackboard
bestBlackboard bm = best
  where hardVecs = M.mapWithKey (\k _ -> hardVec bm k) bm
        softVecs = M.mapWithKey (\k _ -> softVec bm k) bm
        lengths = M.mapWithKey (\k _ -> durationOfCounterPoint k) bm
        oneVec = M.intersectionWith (\as bs -> VV (concat (zipWith (\x y -> [x,y]) as bs))) 
                                      hardVecs softVecs
        together = M.intersectionWith (,) oneVec lengths
        asList = M.assocs together
        best = fst . L.maximumBy (comparing snd) $ asList

-- Define the ordering of two violation vectors.
-- We want less violations, so smaller numbers are better.
-- If we run off one list, we consider it to be extended with 0's.
newtype ViolationVec = VV [Violation] deriving Eq
instance Ord ViolationVec where
  compare (VV []) (VV []) = EQ
  compare (VV []) (VV (b:bs)) | b == 0 = comparing VV [] bs
                              | otherwise = GT
  compare (VV (a:as)) (VV []) | a == 0 = comparing VV as []
                              | otherwise = LT
  compare (VV (a:as)) (VV (b:bs)) | a == b = comparing VV as bs
                                  | otherwise = comparing negate a b

-- collect a property vector from the tree
propVec :: Num a => (BlackboardContext -> a) -> BlackboardMap -> Blackboard -> [a]
propVec f bm b = let cxt = bm M.! b in f cxt : merge (map (propVec f bm) (children cxt))
  where merge = foldl merge2 [] 
        merge2 [] ys = ys
        merge2 xs [] = xs
        merge2 (x:xs) (y:ys) = x + y : merge2 xs ys

hardVec = propVec hard
softVec = propVec soft

-- is it long enough?
longEnough :: Blackboard -> Bool
longEnough b = durationOfCounterPoint b == durationOfCantusFirmus b

-- are there more tests?
outOfTests :: Blackboard -> BlackboardMap -> Bool
outOfTests b bm = null . toRun . (M.! b) $ bm

-- run the next test
applyTest :: Blackboard -> BlackboardMap -> BlackboardMap
applyTest b bm = M.adjust (\bc -> bc { hard = newHard, soft = newSoft, toRun = tests }) b bm
  where cxt = bm M.! b
        (tl:tests) = toRun cxt
        agent = testAgent tl
        result = operate agent (lookAt b (testTime tl))
        newHard = hard cxt + if isHardRule agent && not (testResult result) then 1 else 0
        newSoft = soft cxt + if isSoftRule agent && not (testResult result) then 1 else 0

-- queue up more tests
addTests :: Blackboard -> [TestLoc] -> BlackboardMap -> BlackboardMap
addTests b tests = M.adjust (\cxt -> cxt { toRun = toRun cxt ++ tests }) b

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
controlLoop cc | longEnough best && outOfTests best (blackboards cc) = cc
               | outOfTests best (blackboards cc) = controlLoop (applyGen cc' genAgent)
               | otherwise = controlLoop $ cc { blackboards = applyTest best (blackboards cc) }
  where genAgent = generators cc !! rint
        (rint, newGen) = randomR (0, length (generators cc) - 1) (randGen cc)
        cc' = cc { randGen = newGen }
        best = bestBlackboard (blackboards cc)

-- apply a generator to a blackboard
applyGen :: ControlContext -> Agent -> ControlContext
applyGen cc gen = cc { blackboards = modded, randGen = rGen newBoard }
  where best = bestBlackboard (blackboards cc)
        newBoard = operate gen (setGen best (randGen cc))
        changedTimes = findChangesInCP best newBoard
        tests = TL <$> changedTimes <*> testers cc
        modded = trace (show . focus . front . counterPoint $ newBoard) addTests newBoard tests . insertWithParent best newBoard $ blackboards cc

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
