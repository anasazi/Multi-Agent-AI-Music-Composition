module BlackboardSystem.Blackboard
( Blackboard
, cantusFirmus, scale
, counterPoint, testResult, timeToTestAt
, passTest, failTest
, modifyCP, lookAt
, create
, durationOfCantusFirmus, durationOfCounterPoint
) where

import Music.Voice
import Music.Scale
import Util.Zipper
import System.Random (StdGen)

data Blackboard = Blackboard
                { cantusFirmus :: VoiceZipper -- the read only source voice
                , scale :: Scale -- the scale the piece is composed in
                , counterPoint :: VoiceZipper -- the voice we're making
                , testResult :: Bool -- did we pass this test?
                , timeToTestAt :: Double -- where should we look?
                , randGen :: StdGen -- used for generating random numbers without side effects
                } deriving Show

create cf s rg = Blackboard { cantusFirmus = cf
                            , scale = s
                            , counterPoint = enterFront []
                            , testResult = False 
                            , timeToTestAt = 0
                            , randGen = rg
                            }

lookAt bb t = bb { timeToTestAt = t }

passTest bb = bb { testResult = True }
failTest bb = bb { testResult = False }

modifyCP bb f = bb { counterPoint = f (counterPoint bb) }

durationOfCantusFirmus = durationOfVoice . cantusFirmus
durationOfCounterPoint = durationOfVoice . counterPoint
