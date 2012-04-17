module Control
( makeControl, controlLoop
, ControlContext(..)
, BlackboardContext(..)
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

type Time = Double

data TestLoc = TL Time Agent deriving Show
testTime (TL x _) = x
testAgent (TL _ x) = x

data BlackboardContext = BlackboardContext
 { board :: Blackboard
 , hardViolations :: Integer
 , softViolations :: Integer
 , testsToRun :: [ TestLoc ]
 } deriving Show

instance Eq BlackboardContext where
  a == b = hardEq a b && softEq a b && durEq a b
    where hardEq = (==) `on` hardViolations
          softEq = (==) `on` softViolations
          durEq  = (==) `on` (durationOfCounterPoint . board)

instance Ord BlackboardContext where
  compare a b | hard a > hard b = LT
              | hard a == hard b && soft a > soft b = LT
              | hard a == hard b && soft a == soft b && len a < len b = LT
              | hard a == hard b && soft a == soft b && len a == len b = EQ
              | otherwise = case compare b a of LT -> GT; GT -> LT; EQ -> EQ
    where len = durationOfCounterPoint . board
          soft = softViolations
          hard = hardViolations

data ControlContext = ControlContext
  { testers :: [ Agent ]
  , generators :: [ Agent ]
  , targetDuration :: Time
  , blackboards :: [ BlackboardContext ]
  , randGen :: StdGen
  } deriving Show

makeControl :: [Agent] -> Scale -> Voice -> StdGen -> ControlContext
makeControl agents scale source rg = ControlContext testers generators targetDur blackboards rg
  where testers = filter isTester agents
        generators = filter isGenerator agents
        targetDur = durationOfVoice source
        basedscale = scale .$ (head . focus . front $ source)
        blackboards = [ BlackboardContext (create source basedscale rg) 0 0 [] ]

controlLoop :: ControlContext -> ControlContext
controlLoop cc 
  | let b = bestBlackboard cc in longEnough b && outOfTests b = cc
  | outOfTests (bestBlackboard cc) = controlLoop (applyGen cc' genAgent)
  | otherwise = controlLoop (let best = bestBlackboard cc
                                 rest = filter (/=best) (blackboards cc)
                             in  cc { blackboards = applyTest best : rest })
  where genAgent = generators cc !! rint
        (rint, newGen) = randomR (0, length (generators cc) - 1) (randGen cc)
        cc' = cc { randGen = newGen }
          

bestBlackboard :: ControlContext -> BlackboardContext
bestBlackboard = maximum . blackboards

-- apply a generator to the top rated blackboard
applyGen :: ControlContext -> Agent -> ControlContext
applyGen cc gen = cc { blackboards = modded : blackboards cc, randGen = rGen newBoard }
  where best = bestBlackboard cc
        newBoard = operate gen (setGen (board best) (randGen cc))
        changedTimes = findChangesInCP (board best) newBoard
        tests = TL <$> changedTimes <*> testers cc
        modded = best { board = newBoard, testsToRun = testsToRun best ++ tests }

-- run the next test in the queue
applyTest :: BlackboardContext -> BlackboardContext
applyTest bc = bc { hardViolations = hardNum, softViolations = softNum, testsToRun = tests }
  where result = operate agent (lookAt (board bc) (testTime tl))
        (tl:tests) = testsToRun bc
        agent = testAgent tl
        hardNum | isHardRule agent && not (testResult result) = 1 + hardViolations bc
                | otherwise = hardViolations bc
        softNum | isSoftRule agent && not (testResult result) = 1 + softViolations bc
                | otherwise = softViolations bc

addTests :: BlackboardContext -> [TestLoc] -> BlackboardContext
addTests bc tests = bc { testsToRun = testsToRun bc ++ tests }

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

-- if out counterpoint is just as long as the cantus firmus, than we're done.
longEnough :: BlackboardContext -> Bool
longEnough cxt = let b = board cxt in durationOfCounterPoint b == durationOfCantusFirmus b

-- if we're out of tests than we can apply a generator
outOfTests :: BlackboardContext -> Bool
outOfTests = null . testsToRun
