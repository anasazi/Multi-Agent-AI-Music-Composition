module Blackboard
( Blackboard
, cantusFirmus, theScale, counterPoint, rGen, timeToTestAt, testResult
, create
, setGen, lookAt, passTest, failTest, modifyCP
, durationOfCantusFirmus, durationOfCounterPoint
) where

import Voice
import Scale
import System.Random (StdGen)
import Data.Ord (comparing)

data Blackboard = Blackboard
  { cantusFirmus :: Voice -- the read only source voice
  , theScale :: BasedScale -- the scale the piece is composed in
  , counterPoint :: Voice -- the voice we're making
  , rGen :: StdGen -- used for generating random numbers in generators
  , timeToTestAt :: Double -- where we shoud look
  , testResult :: Bool -- do we pass the test?
  } deriving Show

{- For the purposes of placing Blackboards in a Map, they need an Ord instance.
   Since the only thing we really care about is the counterpoint, that's what we'll use -}
instance Eq Blackboard where
  a == b = counterPoint a == counterPoint b
instance Ord Blackboard where
  compare = comparing counterPoint 

create cf bs rg = Blackboard 
  { cantusFirmus = cf
  , theScale = bs
  , counterPoint = atStart []
  , rGen = rg
  , timeToTestAt = 0
  , testResult = False
  }

setGen bb rg = bb { rGen = rg }
lookAt bb t = bb { timeToTestAt = t }

passTest bb = bb { testResult = True }
failTest bb = bb { testResult = False }

modifyCP bb f = bb { counterPoint = f (counterPoint bb) }

durationOfCantusFirmus = durationOfVoice . cantusFirmus
durationOfCounterPoint = durationOfVoice . counterPoint
