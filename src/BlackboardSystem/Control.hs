module BlackboardSystem.Control
( makeControl, controlLoop
) where

import BlackboardSystem.Blackboard hiding (randGen)
import BlackboardSystem.KnowledgeSource
import Util.Zipper
import Music.Voice
import Music.Note 
import Music.Scale
import qualified Music.Duration as D

import qualified Data.List as L
import Data.Function (on)
import Music.Pitch (point)

import Control.Applicative
import System.Random

{-
The control knows a list of test agents and a list of generator agents. DONE
It keeps a sorted list of blackboard (longest with least errors on top).
It also keeps a map from blackboards to test results and needed tests.
If the top element has passed all the tests, it send it to a random generator.
It compares the old blackboard to the new and identifies what elements changed and need new tests.
If the top element needs a test done, it sends it to that agent.
Once the agent returns, it reads the test result from the blackboard and stores it.
Blackboards returned from agents are placed in the sorted list properly.
If the top element is long enough to meet the goal length, it returns that element and stops.
-}

-- DONE how to represent a needed test? perhaps a list of (time to test, agent to test with)?
-- DONE function of identify where changes occurred on the blackboard. maybe a list of times?
-- DONE applying a test and recording the results
-- DONE applying a generator and recording the results
-- DONE testing whether we should apply a generator or not
-- DONE general control loop

type Time = Double

data ControlContext = ControlContext
  { testers :: [ KnowledgeSource ] -- test agents
  , generators :: [ KnowledgeSource ] -- generator agents
  , targetDuration :: Time -- the length we're looking for and will stop at
  , blackboards :: [ BlackboardContext ] -- all our partial solutions, sorted.
  , randGen :: StdGen -- used for generating random numbers without side effects
  }

{- Creates a control context from a list of agents, a scale to work in, and a source voice.
   The context starts with one blackboard, which has an empty counterpoint and no tests queued.
   Thus, the first action will be generating the start of the counterpoint. -}
makeControl :: [ KnowledgeSource ] -> Scale -> VoiceZipper -> StdGen -> ControlContext
makeControl agents scale source rg = ControlContext testers generators targetDur blackboards rg
  where testers = filter isTester agents
        generators = filter isGenerator agents
        targetDur = durationOfVoice source
        blackboards = [ BlackboardContext (create source scale rg) 0 0 [] ]

{- Main control loop 
  If the best blackboard has passed all tests and is long enough, we're done.
  If the best blackboard has passed all tests and is not long enough, apply a generator to it.
  If the best blackboard needs tests done, apply a test to it.
-}
controlLoop :: ControlContext -> ControlContext
controlLoop cc | let b = bestBlackboard cc in longEnough b && outOfTests b = cc
               | outOfTests (bestBlackboard cc) = controlLoop (applyGen cc' genAgent)
               | otherwise = controlLoop (let (best:rest) = blackboards cc in cc { blackboards = applyTest best : rest })
  where genAgent = generators cc !! rint
        (rint, newGen) = randomR (0, length (generators cc)) (randGen cc)
        cc' = cc { randGen = newGen }
                                             

bestBlackboard = head . blackboards

{- Apply a generator to the top rated blackboard and store the results -}
applyGen :: ControlContext -> KnowledgeSource -> ControlContext
applyGen cc gen = cc { blackboards = reverse (L.sort (modded:rest)) }
  where (best:rest) = blackboards cc
        newBoard = (operate gen) (board best)
        changedTimes = findChangesInCP (board best) newBoard
        tests = TestLoc <$> changedTimes <*> (testers cc)
        modded = best { board = newBoard, testsToRun = tests ++ testsToRun best }

{- Given a blackboard, run the next test in the queue and store the results -}
applyTest :: BlackboardContext -> BlackboardContext
applyTest bc = bc { hardViolations = hardNum, softViolations = softNum, testsToRun = tests }
  where result = (operate agent) (lookAt (board bc) (testTime tl))
        (tl:tests) = testsToRun bc
        agent = testAgent tl
        hardNum | isHardRule agent && testResult result = 1 + hardViolations bc
                | otherwise = hardViolations bc
        softNum | isSoftRule agent && testResult result = 1 + softViolations bc
                | otherwise = softViolations bc

addTests bc tests = bc { testsToRun = tests ++ testsToRun bc }

data TestLoc = TestLoc { testTime :: Time, testAgent :: KnowledgeSource }

data BlackboardContext = BlackboardContext
  { board :: Blackboard
  , hardViolations :: Integer -- number of hard rules broken
  , softViolations :: Integer -- number of soft rules broken
  , testsToRun :: [ TestLoc ]
  }

-- two blackboards have equal contexts if the partial composition is the same length
-- and they have the same number of rule violations
instance Eq BlackboardContext where
  a == b = hardViolations a == hardViolations b && 
           softViolations a == softViolations b &&
           durationOfCounterPoint (board a) == durationOfCounterPoint (board b)

-- a context is better (bigger) than another if:
-- it has less hard rule violations
-- is has the same number of hard rule violations, but less soft rule violations
-- is has the same number of rule violations of both types, but is longer
instance Ord BlackboardContext where        
  a <= b = hard a > hard b || -- if a has more hard rule violations, it is worse
           hard a == hard b && soft a > soft b || -- if a breaks more soft rules than b it is worse
           hard a == hard b && soft a == soft b && length a <= length b
    where length = durationOfCounterPoint . board
          soft = softViolations
          hard = hardViolations

findChangesInCP old new = findChanges (counterPoint old) (counterPoint new)

-- find where changes occured. 
-- Identifies the first divergence point; everything afterwards needs to be rechecked.
findChanges :: VoiceZipper -> VoiceZipper -> [Time]
findChanges old new = let oldNotes = escape old
                          newNotes = escape new
                          oldTimes = scanl (+) 0 $ map (D.dur . dur) oldNotes
                          newTimes = scanl (+) 0 $ map (D.dur . dur) newNotes
                          old' = zip oldTimes oldNotes
                          new' = zip newTimes newNotes
                          eq (t,n) (t',n') = t == t' 
                                          && n == n'
                                          && ((==) `on` pitch) n n'
                                          && ((==) `on` (point . pitch)) n n'
                          changed = L.deleteFirstsBy eq new' old'
                      in map fst changed

-- true if the partial composition is long enough that we're done
longEnough cxt = let b = board cxt in durationOfCounterPoint b == durationOfCantusFirmus b

-- we can apply a generator if we have no tests to run
outOfTests = null . testsToRun
