module BlackboardSystem.Blackboard
( Blackboard
, cantusFirmus, scale
, counterPoint
, passTest, failTest
, modifyCP
) where

import Music.Voice
import Music.Scale
import Util.Zipper

data Blackboard = Blackboard
                { cantusFirmus :: VoiceZipper -- the read only source voice
                , scale :: Scale -- the scale the piece is composed in
                , counterPoint :: VoiceZipper -- the voice we're making
                , testResult :: Bool -- did we pass this test?
                }

passTest bb = bb { testResult = True }
failTest bb = bb { testResult = False }

modifyCP bb f = bb { counterPoint = f (counterPoint bb) }
