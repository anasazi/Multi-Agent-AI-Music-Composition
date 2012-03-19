module BlackboardSystem.Control
(
) where

import BlackboardSystem.Blackboard
import BlackboardSystem.KnowledgeSource
import Util.Zipper
import Music.Voice

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
-- TODO function of identify where changes occurred on the blackboard. maybe a list of times?
-- TODO applying a test and recording the results
-- TODO applying a generator and recording the results
-- DONE testing whether we should apply a generator or not

type Time = Double

data ControlArgs = ControlArgs
  { testers :: [ KnowledgeSource ]
  , generators :: [ KnowledgeSource ] 
  , sourceVoice :: VoiceZipper -- the cantus firmus
  }

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

-- true if the partial composition is long enough that we're done
longEnough cxt = let b = board cxt in durationOfCounterPoint b == durationOfCantusFirmus b

-- we can apply a generator if we have no tests to run
outOfTests = null . testsToRun
