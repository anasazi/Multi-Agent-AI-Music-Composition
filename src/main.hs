import qualified FirstSpecies
import qualified GeneralSpecies

import Control
import Agent

import Note
import Scale
import Voice

import System.Random

cantusfirmus = atStart [ makeNote D 3 0 0 0
                       , makeNote F 3 0 0 0
                       , makeNote G 3 0 0 0
                       , makeNote A 3 0 0 0
                       , makeNote G 3 0 0 0
                       , makeNote F 3 0 0 0
                       , makeNote E 3 0 0 0
                       , makeNote D 3 0 0 0
                       , makeNote F 3 0 0 0
                       , makeNote G 3 0 0 0
                       , makeNote E 3 0 0 0
                       , makeNote D 3 0 0 0
                       ]

allAgents = GeneralSpecies.agents ++ FirstSpecies.agents

control gen = makeControl allAgents major cantusfirmus gen

main = do 
          gen <- getStdGen
          --let gen = mkStdGen 1
          let cc = (controlLoop (control gen))
          --print $ targetDuration cc
          print (maximum (blackboards cc))
          --mapM_ print (blackboards cc)
