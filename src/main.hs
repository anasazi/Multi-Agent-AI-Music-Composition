import qualified FirstSpecies
import qualified GeneralSpecies

import BlackboardSystem.Control
import BlackboardSystem.KnowledgeSource

import Music.Note
import Music.Interval
import Music.Duration
import Music.Scale
import Util.Zipper

import System.Random

cantusfirmus = enterFront [ midC
                          , wrapWithDur (maj3 #^ (pitch midC)) whole
                          , wrapWithDur (perf5 #^ (pitch midC)) whole
                          , wrapWithDur (perf4 #^ (pitch midC)) whole
                          , wrapWithDur (maj2 #^ (pitch midC)) whole
                          , wrapWithDur (maj6 #^ (pitch midC)) whole
                          , wrapWithDur (perf5 #^ (pitch midC)) whole
                          , wrapWithDur (maj3 #^ (pitch midC)) whole
                          , midC
                          ]

allAgents = GeneralSpecies.agents ++ FirstSpecies.agents

control gen = makeControl allAgents major cantusfirmus gen

main = do 
          gen <- getStdGen
          --let gen = mkStdGen 1
          let cc = (controlLoop (control gen))
          print $ targetDuration cc
          mapM_ print (blackboards cc)
