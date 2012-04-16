module Blackboard
( Blackboard
, cantusFirmus, theScale, counterPoint, randGen, timeToTestAt, testResult
, create
, setGen, lookAt, passTest, failTest, modifyCP
, durationOfCantusFirmus, durationOfCounterPoint
) where

import Voice
import Scale
import System.Random (StdGen)

data Blackboard = Blackboard
  { cantusFirmus :: Voice -- the read only source voice
  , theScale :: BasedScale -- the scale the piece is composed in
  , counterPoint :: Voice -- the voice we're making
  , randGen :: StdGen -- used for generating random numbers in generators
  , timeToTestAt :: Double -- where we shoud look
  , testResult :: Bool -- do we pass the test?
  } deriving Show

create cf bs rg = Blackboard 
  { cantusFirmus = cf
  , theScale = bs
  , counterPoint = atStart []
  , randGen = rg
  , timeToTestAt = 0
  , testResult = False
  }

setGen bb rg = bb { randGen = rg }
lookAt bb t = bb { timeToTestAt = t }

passTest bb = bb { testResult = True }
failTest bb = bb { testResult = False }

modifyCP bb f = bb { counterPoint = f (counterPoint bb) }

durationOfCantusFirmus = durationOfVoice . cantusFirmus
durationOfCounterPoint = durationOfVoice . counterPoint
