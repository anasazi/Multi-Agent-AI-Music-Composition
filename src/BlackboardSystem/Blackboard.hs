module BlackboardSystem.Blackboard
( Blackboard
, cantusFirmus
, counterPoint
, passTest, failTest
, modifyCP
) where

import Music.Voice
import Util.Zipper

data Blackboard = Blackboard
                { cantusFirmus :: VoiceZipper
                , counterPoint :: VoiceZipper
                , testResult :: Bool
                }

passTest bb = bb { testResult = True }
failTest bb = bb { testResult = False }

modifyCP bb f = bb { counterPoint = f (counterPoint bb) }
